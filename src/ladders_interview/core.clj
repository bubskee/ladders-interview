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

;;-------------------------------------------------------------------

;; core.async pipeline method


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

