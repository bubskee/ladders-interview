 (ns ladders-interview.data-generation
     (:require [clojure.spec :as s]
               [clojure.spec.gen :as gen]))


(def email-domains
  #{"indeediot.com"
    "monstrous.com"
    "linkedarkpattern.com"
    "dired.com"
    "lice.com"
    "careershiller.com"
    "glassbore.com"})

(def email-regex
  #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")

(s/def ::email-address
  (s/with-gen
    (s/and string? #(re-matches email-regex %))
    #(->>
       (gen/tuple (gen/such-that not-empty (gen/string-alphanumeric))
                  (s/gen email-domains))
       (gen/fmap (fn [[addr domain]] (str addr "@" domain))))))

(s/def ::spam-score
  (s/double-in :min 0 :max 1))

(s/def ::email-record
  (s/keys :req-un [::email-address ::spam-score]))


(defn n-thousand-email-records [^Integer n]
  "takes a number, n, returns a list of precisely n-thousand email records"
  (flatten (for [x (range (* 100 n))]
            (gen/sample (s/gen ::email-record)))))

;; extending spec for more regular data
;; completely regular data follows

 (s/def ::spam-score1d20
   (s/double-in :min 0 :max 0.05))

(s/def ::spam-score2d20
  (s/double-in :min 0.05 :max 0.1))

(s/def ::spam-score3d20
  (s/double-in :min 0.1 :max 0.15))

(s/def ::spam-score4d20
  (s/double-in :min 0.15 :max 0.2))

(s/def ::spam-score5d20
  (s/double-in :min 0.2 :max 0.25))

(s/def ::spam-score6d20
  (s/double-in :min 0.25 :max 0.3))

(s/def ::spam-score7d20
  (s/double-in :min 0.3 :max 0.35))

(s/def ::spam-score8d20
  (s/double-in :min 0.35 :max 0.4))

(s/def ::spam-score9d20
  (s/double-in :min 0.4 :max 0.45))

(s/def ::spam-score10d20
  (s/double-in :min 0.45 :max 0.5))

(s/def ::spam-score11d20
  (s/double-in :min 0.5 :max 0.55))

(s/def ::spam-score12d20
  (s/double-in :min 0.55 :max 0.6))

(s/def ::spam-score13d20
  (s/double-in :min 0.6 :max 0.65))

(s/def ::spam-score14d20
  (s/double-in :min 0.65 :max 0.7))

(s/def ::spam-score15d20
  (s/double-in :min 0.7 :max 0.75))

(s/def ::spam-score16d20
  (s/double-in :min 0.75 :max 0.8))

(s/def ::spam-score17d20
  (s/double-in :min 0.8 :max 0.85))

(s/def ::spam-score18d20
  (s/double-in :min 0.85 :max 0.9))

(s/def ::spam-score19d20
  (s/double-in :min 0.9 :max 0.95))

(s/def ::spam-score20d20
  (s/double-in :min 0.95 :max 1))

;;
