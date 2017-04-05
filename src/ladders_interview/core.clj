(ns ladders-interview.core
  (:require [ladders-interview.data-generation :as d-g]
            [medley.core :refer [distinct-by]]))

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

(defn xduce-rm&rm-l100
  "stateful transducer which filters em-recs based on running mean of their :spam-score

  considers both the total running mean, and the running mean of the last 100 items"
  ([]
   (fn [xf]
     (let [l-hund! (atom clojure.lang.PersistentQueue/EMPTY)
           running-total! (atom 0)
           running-count! (atom 0)]
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
                (< 0.05 (/ new-r-total
                          new-r-count))
                (< 0.1 (mean new-last100))
                   )
              (do (reset! l-hund! new-last100)
                  (reset! running-total! new-r-total)
                  (reset! running-count! new-r-count)
                  result)
              (xf result input))))))))
  ([coll] (sequence (xduce-rm&rm-l100) coll)))

(defn quick-n-dirty [em-recs]
  (->> em-recs
       (remove #(< 0.15 (:spam-score %)) )
       (distinct-by :email-address)
       (partition-all 10000)
       (mapcat xduce-rm&rm-l100)
       (flatten)))


(comment
  ;; REPL stuff
  (def data (d-g/n-thousand-email-records 50))
  (def rdata (d-g/n-thousand-reg-em-recs 50))




  (->> data
       (remove high-spam?)
       (map :spam-score)
       (mean))


  ;; scratchpad

  (defn running-means-check [l100-vol total-vol {spam-score :spam-score }]
    (let [old-l100 (:last100 @l100-vol)
          new-l100 (queue-100-step old-l100 spam-score)
          new-l100-rm (mean new-l100)
          old-totals @total-vol
          new-total-count (inc (:total-count old-totals))
          new-total-spam (+ spam-score
                            (:total-spam old-totals))
          new-total-rm (/ new-total-spam
                          new-total-count)]
      ))


  )

