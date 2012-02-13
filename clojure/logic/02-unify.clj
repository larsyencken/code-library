(ns logic-02
  (:require [clojure.core.logic :as logic]))

(println
  (logic/run 1 [q]
       (logic/== 1 q)))
