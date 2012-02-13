(ns logic-06
  (:require [clojure.core.logic :as logic]))

(defn succ [p n]
  (logic/conso p [] n))

(def zero 0)
(def one '(0))

(defn natural-number [x]
  (logic/conde
    ((logic/== x zero))
    ((logic/fresh [previous]
      (succ previous x)
      (natural-number previous)))))

(println
  (logic/run 1 [q]
    (natural-number one)))

(println
  (logic/run 6 [q]
    (natural-number q)))

