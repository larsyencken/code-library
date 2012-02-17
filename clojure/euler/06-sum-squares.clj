#!/usr/bin/env clj
;
;  06-sum-squares.py
;  Code
;
;  Created by Lars Yencken on 2012-02-17.
;  Copyright 2012 Lars Yencken. All rights reserved.
;

(ns euler
  (:use clojure.test))

(defn sum-sq [xs]
  (reduce + (map #(* % %) xs)))

(deftest sum-sq-1
  (is (= (sum-sq (range 1 4)) 14)))

(defn sq-sum [xs]
  (let [sum (reduce + xs)]
    (* sum sum)))

(deftest sq-sum-1
  (is (= (sq-sum (range 1 4)) 36)))

(deftest problem-test
  (is (= (- (sq-sum (range 1 101)) (sum-sq (range 1 101))) 25164150)))

(run-tests)
(println)
(println "Answer:"
  (-
    (sq-sum (range 1 101))
    (sum-sq (range 1 101))))
