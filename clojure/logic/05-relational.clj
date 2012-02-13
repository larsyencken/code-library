(ns logic-05
  (:require [clojure.core.logic :as logic]))

(defn succ [p n]
  (logic/conso p [] n))

(def zero 0)
(def one '(0))

(println
  (logic/run 1 [q]
    (succ zero q)))

(println
  (logic/run 1 [q]
    (succ q one)))

