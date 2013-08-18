#lang refined-acl2/core

(require refined-acl2/prelude/base/match)

(description DATA
  (~define (f x))
  (define-match-type (f (f x))))

(description IN
  (~component Data #:> DATA)
  (~open Data))

(description OUT)

(generic (Gen [I #:> IN]) #:> OUT
  (use I))
