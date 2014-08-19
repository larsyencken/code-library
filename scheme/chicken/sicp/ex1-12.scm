; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html
;
; Write a procedure that computes elements of Pascal's triangle by means of
; a recursive process.
;

(define (choose n k)
  (if (or (= k 0) (= k n))
    1
    (+
      (choose (- n 1) (- k 1))
      (choose (- n 1) k))))
