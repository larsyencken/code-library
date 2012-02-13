(ns logic-01
  (:require [clojure.core.logic :as logic]))

(println
  (logic/run 1 [q]
       logic/succeed))
