#!/usr/bin/env clj
;
;  04-palindrome.py
;  code
;
;  Created by Lars Yencken on 2012-01-17.
;  Copyright 2012 Lars Yencken. All rights reserved.
;

(ns euler
  (:require clojure.contrib.combinatorics))

(defn ispalindrome? [x]
  (let [s (str x)]
    (= s (apply str (reverse s)))))

(println
  (reduce max
    (filter #(ispalindrome? %)
            (map #(* (first %) (second %))
                 (clojure.contrib.combinatorics/combinations 
                   (range 100 1000) 2)))))
