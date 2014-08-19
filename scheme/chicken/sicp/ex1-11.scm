; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html
;
; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
;

(define (f n)
  (if (< n 3)
    n
    (+
      (f (- n 1))
      (* 2 (f (- n 2)))
      (* 3 (f (- n 3))))))
; this version is recursive


(define (f2-iter n v2 v1 v)
  (if (< n 3)
    v
    (f2-iter (- n 1)
             v1
             v
             (+ v (* 2 v1) (* 3 v2)))))

(define (f2 n)
  (f2-iter n 0 1 2))
; this version is iterative
