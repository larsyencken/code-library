#!/usr/bin/env clj
;
;  03-prime-factors.clj
;
;  Calculate prime factors of a number.
;

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

(deftest get-factors-1
         (is (= [2 3 5] (get-factors 30))))

(deftest get-factors-2
         (is (= [2 2 2 2 2 3] (get-factors 96))))

(deftest get-factors-solution
         (is (= 6857 (reduce max (get-factors 600851475143)))))

(run-tests)

(println)
(println "Answer:" (reduce max (get-factors 600851475143)))
