#!/usr/bin/env clj
;
;  01-sum-numbers.clj
;
;  Sum all numbers from 1 to 1000 which are multiples of 3 or 5.
;

(ns euler
  (:use clojure.test))

(defn sum [xs]
  (reduce + 0 xs))

(defn m35 [x]
  (or (= 0 (mod x 3)) (= 0 (mod x 5))))

(defn sum-numbers [n]
  "Sum all multiples of 3 and 5 between 1 and n (inclusive)."
  (let [xs (range 1 n)]
    (let [ns (filter m35 xs)]
      (sum ns))))

(deftest test-sum-numbers-1
         (is (= 23 (sum-numbers 10))))

(deftest test-sum-numbers-2
         (is (= 233168 (sum-numbers 1000))))

(run-tests)

(println)
(println "Answer:" (sum-numbers 1000))
