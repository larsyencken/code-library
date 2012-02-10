#!/usr/bin/env clj
;
;  05-divisible.py
;  Code
;
;  Created by Lars Yencken on 2012-01-29.
;  Copyright 2012 99designs. All rights reserved.
;

; 2520 is the smallest number that can be divided by each of the numbers from
; 1 to 10 without any remainder. What is the smallest positive number that is
; evenly divisible by all of the numbers from 1 to 20?

(ns euler
  (:use clojure.test))

(defn get-factor
  [x] (first (filter #(= 0 (mod x %)) (range 2 (+ 1 x)))))

(defn get-factors-acc [x acc]
  (if (= x 1)
    acc
    (let
      [f (get-factor x)]
      (recur (/ x f) (conj acc f)))))

(defn get-factors [x]
  (get-factors-acc x []))

(deftest test-get-factors-1
         (is (= [2 3 5] (get-factors 30))))

(deftest test-get-factors-2
         (is (= [2 2 2 2 2 3] (get-factors 96))))

(defn merge-dists [xs ys]
  "Merge two frequency distributions taking the maximum count for each key."
  (let [keys (list* (concat (keys xs) (keys ys)))]
    (zipmap
      keys
      (map #(max (get xs % 0) (get ys % 0)) keys))))

(defn pow [n i]
  (reduce * (repeat i n)))

(deftest test-pow-1
         (is (= 8 (pow 2 3))))

(deftest test-pow-2
         (is (= 7 (pow 7 1))))

(defn prod-dist [d]
  "Calculate the product of the factor distribution."
  (reduce * (map #(pow (% 0) (% 1)) (seq d))))

(deftest test-prod-dist
         (is (= 96 (prod-dist {2 5, 3 1}))))

(defn smallest-divisible [n]
  (prod-dist
    (reduce
      merge-dists
      (map #(frequencies (get-factors %)) (range 2 (+ n 1))))))

(deftest test-smallest-divisible-1
         (is (= 2520 (smallest-divisible 10))))

(run-tests)
(println)
(println "Answer:" (smallest-divisible 20))

