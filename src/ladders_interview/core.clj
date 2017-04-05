(ns ladders-interview.core
  (:require [ladders-interview.data-generation :as d-g]
            [medley.core :refer [distinct-by]]
            [fn-fx.fx-dom :as fx-dom]
            [fn-fx.diff :refer [component defui render]]
            [fn-fx.controls :as controls]
            [fn-fx.util :as util]
            [fn-fx.diff :as diff]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:import (javafx.stage FileChooser)
           (javafx.scene.chart.XYChart)
           (javafx.beans.property ReadOnlyObjectWrapper)))


;; We need you to write a system that will process our email batches and
;; decide whether or not to send each email based on the following rules:

;; 1. Must send a maximum of one email per address.
;; 2. Must never send an email with a spam score of more than 0.3.
;; 3. The running mean of spam ratings must never get above 0.05.
;; 4. The mean spam score of the most recent 100 emails sent can't go
;;    above 0.1.


(defn mean [xs]
  (/ (reduce + xs) (.size xs)))


(defn queue-100-step [q n]
  (if (< (.size q) 100)
    (conj q n)
    (conj (pop q) n)))

;; stateful xducer method

(defn xduce-rm&rm-l100
  "stateful transducer which filters em-recs based on running mean of their :spam-score

  considers both the total running mean, and the running mean of the last 100 items"
  ([]
   (fn [xf]
     (let [l-hund! (volatile! clojure.lang.PersistentQueue/EMPTY)
           running-total! (volatile! 0)
           running-count! (volatile! 0)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result input]
          (let [last100 @l-hund!
                r-total @running-total!
                r-count @running-count!
                new-last100 (queue-100-step last100 (:spam-score input))
                new-r-count (inc r-count)
                new-r-total (+ r-total (:spam-score input))]
            (if
              (and
                (< 0.04 (/ new-r-total
                           new-r-count))
                (< 0.9 (mean new-last100))
                )
              (do (vreset! l-hund! new-last100)
                  (vreset! running-total! new-r-total)
                  (vreset! running-count! new-r-count)
                  result)
              (xf result input))))))))
  ([coll] (sequence (xduce-rm&rm-l100) coll)))

(defn stateful-xducer-method [em-recs]
  (->> em-recs
       (remove #(< 0.3 (:spam-score %)) )
       (distinct-by :email-address)
       (xduce-rm&rm-l100)))

;;-----------------------------------------------------------------------------

;; core.async method

;;-----------------------------------------------------------------------------

;; GUI building


(defn cell-value-factory [f]
  (reify javafx.util.Callback
    (call [this entity]
      (ReadOnlyObjectWrapper. (f (.getValue entity))))))

(defui TableColumn
       (render [this {:keys [index name]}]
               (controls/table-column
                 :text name
                 :cell-value-factory (cell-value-factory #(nth % index)))))

(defui Table
       (render [this {:keys [data]}]
               (controls/table-view
                 :columns (map-indexed
                            (fn [index name]
                              (table-column {:index index
                                             :name name}))
                            (first data))
                 :items (rest data)
                 :placeholder (controls/label
                                :text "Import some data first"))))

(defn data->series [data]
  (if (some? data)
    (let [transpose #(apply mapv vector %)
          transposed-data (transpose data)
          xs (rest (first transposed-data))
          build-series (fn [[name & ys]]
                         (diff/component :javafx.scene.chart.XYChart$Series
                                         {:name name
                                          :data (map
                                                  #(javafx.scene.chart.XYChart$Data.
                                                     (bigdec (first %))
                                                     (bigdec (second %)))
                                                  (transpose [xs ys]))}))]
      (map build-series transposed-data))
    []))

(defui Plot
       (render [this {:keys [data]}]
               (diff/component [:javafx.scene.chart.ScatterChart
                                []
                                [(javafx.scene.chart.NumberAxis.)
                                 (javafx.scene.chart.NumberAxis.)]]
                               {:data (data->series data)})))

(defn force-exit [root-stage?]
  (reify javafx.event.EventHandler
    (handle [this event]
      (when-not root-stage?
        (println "Closing application")
        (javafx.application.Platform/exit)))))

(defui Stage
       (render [this {:keys [root-stage? data options] :as state}]
               (controls/stage
                 :title "fn-fx-ui"
                 :on-close-request (force-exit root-stage?)
                 :shown true
                 :scene (controls/scene
                          :root (controls/border-pane
                                  :top (controls/h-box
                                         :padding (javafx.geometry.Insets. 15 12 15 12)
                                         :spacing 10
                                         :alignment (javafx.geometry.Pos/CENTER)
                                         :children [(controls/button
                                                      :text "Import CSV"
                                                      :on-action {:event :import-csv
                                                                  :fn-fx/include {:fn-fx/event #{:target}}})
                                                    (controls/check-box
                                                      :text "Import first row as headers"
                                                      :selected (get-in options [:csv :first-row-headers])
                                                      :on-action {:event :toggle-option
                                                                  :path [:csv :first-row-headers]})
                                                    (controls/button
                                                      :text "Reset"
                                                      :on-action {:event :reset})])
                                  :center (controls/v-box
                                            :children [(table {:data data})
                                                       (plot {:data data})]))))))

(defmulti handle-event (fn [_ {:keys [event]}]
                         event))

(def initial-state
  {:options {:csv {:first-row-headers false}}
   :root-stage? true
   :data [[]]})

(defonce data-state (atom initial-state))

(defmethod handle-event :reset
  [_ {:keys [root-stage?]}]
  (assoc initial-state :root-stage? root-stage?))

(defmethod handle-event :toggle-option
  [state {:keys [path]}]
  (update-in state (cons :options path) not))

(defmethod handle-event :import-csv
  [{:keys [options] :as state} {:keys [fn-fx/includes]}]
  (let [window (.getWindow (.getScene (:target (:fn-fx/event includes))))
        dialog (doto (FileChooser.) (.setTitle "Import CSV"))
        file (util/run-and-wait (.showOpenDialog dialog window))
        data (with-open [reader (io/reader file)]
               (doall (csv/read-csv reader)))]
    (assoc state :file file :data
                 (if (get-in options [:csv :first-row-headers])
                   data
                   (cons (map #(str "x" (inc %)) (range (count (first data)))) data)))))

(defn start
  ([] (start {:root-stage? true}))
  ([{:keys [root-stage?]}]
   (swap! data-state assoc :root-stage? root-stage?)
   (let [handler-fn (fn [event]
                      (println event)
                      (try
                        (swap! data-state handle-event event)
                        (catch Throwable exception
                          (println exception))))
         ui-state (agent (fx-dom/app (stage @data-state) handler-fn))]

     (add-watch data-state :ui (fn [_ _ _ _]
                                 (send ui-state
                                       (fn [old-ui]
                                         (println "-- State Updated --")
                                         (println @data-state)
                                         (fx-dom/update-app old-ui (stage @data-state)))))))))


(comment
  ;; REPL stuff
  (def data (d-g/n-thousand-email-records 50))
  (def rdata (d-g/n-thousand-reg-em-recs 50))




  (->> data
       (remove high-spam?)
       (map :spam-score)
       (mean))


  ;; scratchpad


  )

