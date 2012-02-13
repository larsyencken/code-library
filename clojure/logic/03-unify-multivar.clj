(ns logic-03
  (:require [clojure.core.logic :as logic]))

(println
  (logic/run 1 [q]
    (logic/fresh [v1]
      (logic/== v1 1)
      (logic/== q v1))))

