#lang mischief

(provide
  (struct-out type)
  ref?)

(require
  refined-acl2/proof/term)

(struct type [ref] #:transparent)

(define (ref? x)
  (or
    (syntax? x)
    (false? x)))
