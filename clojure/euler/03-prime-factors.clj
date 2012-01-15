#!/usr/bin/env clj
;
;  03-prime-factors.clj
;
;  Calculate prime factors of a number.
;

(defn get-factor [x]
  (first (filter #(= 0 (mod x %)) (range 2 (+ 1 x)))))

(defn get-factors-acc [x fs]
  (if (= x 1)
    fs
    (let
      [f (get-factor x)]
      (recur (/ x f) (cons f fs)))))

(defn get-factors [x]
  (get-factors-acc x '()))

(println (reduce max (get-factors 600851475143)))
