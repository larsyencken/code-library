(ns logic-07
  (:require [clojure.core.logic :as logic]))

; append([], Y, Y).
; append([H|T], Y, [H|R]) :- append(T, Y, R).

(logic/defne appendo [x y z]
  ([() _ y])
  ([[a . d] _ [a . r]] (appendo d y r)))

(println
  (logic/run 1 [q]
    (appendo '(1 2) '(3 4) q) ))

(println
  (logic/run 3 [q]
    (logic/fresh [r]
      (appendo q r '(1 2 3 4)))))
