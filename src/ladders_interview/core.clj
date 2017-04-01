(ns ladders-interview.core)

;; We need you to write a system that will process our email batches and
;; decide whether or not to send each email based on the following rules:

;; 1. Must send a maximum of one email per address.
;; 2. Must never send an email with a spam score of more than 0.3.
;; 3. The running mean of spam ratings must never get above 0.05.
;; 4. The mean spam score of the most recent 100 emails sent can't go
;;    above 0.1.



