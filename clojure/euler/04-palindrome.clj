#!/usr/bin/env clj
;
;  04-palindrome.py
;  code
;
;  Created by Lars Yencken on 2012-01-17.
;  Copyright 2012 Lars Yencken. All rights reserved.
;

(ns euler
  (:use clojure.contrib.combinatorics)
  (:use clojure.test))

(defn is-palindrome? [x]
  (let [s (str x)]
    (= s (apply str (reverse s)))))

(deftest is-palindrome-1
         (is (is-palindrome? 101)))

(deftest is-palindrome-2
         (is true (is-palindrome? 20)))

(deftest is-palindrome-3
         (is true (is-palindrome? 403304)))

(run-tests)

(println)
(println
  "Answer:"
  (reduce max
    (filter #(is-palindrome? %)
            (map #(* (first %) (second %))
                 (combinations (range 100 1000) 2)))))
