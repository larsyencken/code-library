;
;  exercises.clj
;
; http://www.cis.upenn.edu/~matuszek/cis554-2011/Assignments/clojure-01-exercises.html

(defn collatz [n]
  "A fancy formula for returning 1 every time."
  (if (= n 1)
    1
    (if (= (mod n 2) 0)
      (recur (/ n 2))
      (recur (+ (* n 3) 1)))))

(defn shallow-reverse-a [xs ys]
  (if (seq xs)
    (recur (rest xs) (cons (first xs) ys))
    ys))

(defn shallow-reverse [xs]
  "Reverse a list."
  (shallow-reverse-a xs ()))

(defn remove-duplicates-a [xs, acc]
  (let [x (first xs)]
    (if (seq xs)
      (recur (filter #(not= x %) (rest xs)) (cons x acc))
      (reverse acc))))

(defn remove-duplicates [xs]
  "Remove duplicates from a list."
  (remove-duplicates-a xs ()))

(defn my-flatten-a [xs acc]
  (if (seq xs)
    (let [x (first xs)]
      (if (seq? x)
        (if (seq x)
          (recur (concat x (rest xs)) acc)
          (recur (rest xs) acc))
        (recur (rest xs) (cons x acc))))
    (reverse acc)))

(defn my-flatten [xs]
  "Flatten a list."
  (my-flatten-a xs ()))

; (= (skeleton ()) ())
; (= (skeleton '(1 2 3) ())
(defn skeleton [xs]
  "Return the nested list structure without the elements."
  (if (seq xs)
    (let [h (first xs) t (rest xs)]
      (if (seq? h)
        (cons (skeleton h) (skeleton t))
        (recur t)))
    xs))

; (= (deep-reverse '(:a (:b :c (:d)) :e)) '(:e ((:d) :c :b) :a))
(defn deep-reverse [xs]
  "Reverses elements of xs at all levels."
  ())
