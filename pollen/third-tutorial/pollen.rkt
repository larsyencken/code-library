#lang racket

(require txexpr pollen/decode pollen/misc/tutorial)

(provide author em root)

(define author "Trevor Goodchild")

(define (em . elements)
  (txexpr 'extra-big empty elements))

(define (root . elements)
  (txexpr 'root empty (decode-elements elements
                                       #:txexpr-elements-proc decode-paragraphs
                                       #:string-proc (compose1 smart-quotes smart-dashes))))

