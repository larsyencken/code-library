#lang racket/base

(require pollen/tag)
(provide (all-defined-out))
(define headline (default-tag-function 'h2))
(define items (default-tag-function 'ul))
(define item (default-tag-function 'li 'p))
(define (link url text) `(a ((href ,url)) ,text))
