#!/usr/bin/env clj
;
;  01-sum-numbers.clj
;
;  Sum all numbers from 1 to 1000 which are multiples of 3 or 5.
;

(defn sum [xs]
  (reduce + 0 xs))

(defn m35 [x]
  (or (= 0 (mod x 3)) (= 0 (mod x 5))))

(defn sum-numbers [n]
  "Sum all multiples of 3 and 5 between 1 and n (inclusive)."
  (let [xs (range 1 1000)]
    (let [ns (filter m35 xs)]
      (println (sum ns)))))

(sum-numbers 1000)
