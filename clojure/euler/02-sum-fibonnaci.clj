#!/usr/bin/env clj
;
;  02-sum-fibbonaci.clj
;
;  Sum all even elements of the fibonacci sequence whose values are less than
;  4 million.
;

(ns euler
  (:use clojure.test))

(defn fibonacci-acc [n np t]
  (let [npp (+ n np)]
    (if (< npp t)
      (cons npp (fibonacci-acc np npp t))
      nil)))

; NOTE: cons is not lazy in clojure
(defn fibonacci [t]
  (cons 1 (cons 2 (fibonacci-acc 1 2 t))))

(defn fibonacci-sum [n]
  (reduce + 0 (filter (fn [x] (= 0 (mod x 2)))
                             (fibonacci n))))

(deftest test-fibonacci-sum-1
  (is (= 2 (fibonacci-sum 6))))

(deftest test-fibonacci-sum-2
  (is (= 4613732 (fibonacci-sum 4000000))))

(run-tests)

(println)
(println "Answer:" (fibonacci-sum 4000000))
