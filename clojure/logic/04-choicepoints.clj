(ns logic-04
  (:require [clojure.core.logic :as logic]))

(println
  (logic/run 2 [q]
    (logic/conde
      ((logic/== q 1))
      (logic/succeed logic/fail)
      (logic/succeed)
      ((logic/== q 2)))))

