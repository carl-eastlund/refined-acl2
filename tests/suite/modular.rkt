#lang mischief

(provide modular-tests)

(require
  rackunit
  mischief/preserve-expensive-metadata
  refined-acl2/tests/harness)

(define modular-tests
  (test-suite "modules"

    (test-suite "provider and client modules"

      (test-refined-acl2 #:name 'provider
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require (only racket/base +))
           (define-values {}
             (begin0 (#%plain-app values)
               (#%plain-lambda {} '#:primitive + '2 'ACL2 'BINARY-+)))
           (define-values {one}
             (#%plain-lambda {}
               '#:function '#:no-measure '#:no-goals
               1))
           (define-values {inc}
             (#%plain-lambda {x}
               '#:function '#:no-measure '#:no-goals
               (+ x (one))))
           (#%provide inc)])
        #:exports '[inc]
        #:checks
        (lambda (inc)
          (check-equal? (inc 2) 3))
        #:proof
        `[(,ASSERT-EVENT
            (,IF
              (,QUOTE ,T)
              (,QUOTE ,T)
              (,BINARY-+
                (,QUOTE 1)
                (,QUOTE 2))))
          (,DEFUN ONE () (,QUOTE 1))
          (,DEFUN INC (X) (,BINARY-+ X (ONE)))])

      (test-refined-acl2 #:name 'requirer
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'provider (only racket/base cons))
           (define-values {}
             (begin0 (#%plain-app values)
               (#%plain-lambda {} '#:primitive cons '2 'ACL2 'CONS)))
           (define-values {one}
             (#%plain-lambda {x}
               '#:function '#:no-measure '#:no-goals
               (cons x '())))
           (one (inc 2))])
        #:results (list '(3))
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "provider"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,SKIP-PROOFS
            (,DEFUN ONE.2 () (,QUOTE 1)))
          (,SKIP-PROOFS
            (,DEFUN INC (X) (,BINARY-+ X (ONE.2))))
          (,ASSERT-EVENT
            (,IF
              (,QUOTE ,T)
              (,QUOTE ,T)
              (,CONS
                (,QUOTE 1)
                (,QUOTE 2))))
          (,DEFUN ONE (X)
            (,CONS X (,QUOTE ,EMPTY)))])

      (test-refined-acl2 #:name 'all-but-z
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-values {x}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals 1))
           (define-values {y}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals 2))
           (define-values {z}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals 3))
           (#%provide (all-defined-except z))])
        #:exports '[x y]
        #:checks (lambda (x y) (check-equal? (x) 1) (check-equal? (y) 2))
        #:proof
        `[(,DEFUN X () (,QUOTE 1))
          (,DEFUN Y () (,QUOTE 2))
          (,DEFUN Z () (,QUOTE 3))])

      (test-refined-acl2 #:name 'x-y-and-z
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require (only 'all-but-z x y) (rename 'all-but-z z x))
           (define-values {a}
             (#%plain-lambda {}
               '#:function '#:no-measure '#:no-goals
               (if (x) (y) (z))))
           (a)])
        #:results (list 2)
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "all-but-z"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,SKIP-PROOFS
            (,DEFUN X () (,QUOTE 1)))
          (,SKIP-PROOFS
            (,DEFUN Y () (,QUOTE 2)))
          (,SKIP-PROOFS
            (,DEFUN Z () (,QUOTE 3)))
          (,DEFUN A () (,IF (X) (Y) (X)))]))

    (test-suite "empty + import"

      (test-refined-acl2 #:name 'empty-module
        #:lang 'refined-acl2/kernel)

      (test-refined-acl2 #:name 'import-empty
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'empty-module)])
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "empty-module"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))]))

    (test-suite "cross-module macros"

      (test-refined-acl2 #:name 'macro-provider
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-syntaxes {private}
             (syntax-parser
               [(_ v) #'(quote v)]))
           (define-syntaxes {quoth}
             (syntax-parser
               [(_ v) #'(private v)]))
           (#%provide quoth)])
        #:exports '[])

      (test-refined-acl2 #:name 'macro-consumer
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'macro-provider)
           (define-values {one}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
               (quoth (unbound names))))])
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "macro-provider"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,DEFUN ONE ()
            (,QUOTE (unbound names . ,EMPTY)))]))

    (test-suite "cross-module higher order macro"

      (test-refined-acl2 #:name 'higher-order-macro-provider
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-syntaxes {define-associative}
             (syntax-parser
               [(_ op:id binop:id zero:expr)
                #'(define-syntaxes {op}
                    (syntax-parser
                      [(_) #'zero]
                      [(_ e:expr . es:expr)
                       #'(binop e (op . es))]))]))
           (#%require (rename racket/base ++ +))
           (define-values {}
             (begin0 (#%plain-app values)
               (#%plain-lambda {} '#:primitive ++ '2 'ACL2 'BINARY-+)))
           (define-associative + ++ 0)
           (#%provide (all-defined))])
        #:proof
        `[(,ASSERT-EVENT
            (,IF
              (,QUOTE ,T)
              (,QUOTE ,T)
              (,BINARY-+
                (,QUOTE 1)
                (,QUOTE 2))))])

      (test-refined-acl2 #:name 'higher-order-macro-consumer
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'higher-order-macro-provider)
           (define-values {six}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
               (+ 1 2 3)))
           (#%provide six)])
        #:exports '(six)
        #:checks
        (lambda (six)
          (check-equal? (six) 6))
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "higher-order-macro-provider"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,DEFUN SIX ()
            (,BINARY-+ (,QUOTE 1)
              (,BINARY-+ (,QUOTE 2)
                (,BINARY-+ (,QUOTE 3)
                  (,QUOTE 0)))))]))

    (test-suite "literals + import"

      (test-refined-acl2 #:name 'module-with-literals
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-syntaxes {same}
             (lambda (stx)
               (define arg (cadr (syntax->list stx)))
               #`(quote #,(free-identifier=? arg #'same))))
           (same same)
           (same diff)
           (#%provide same)])
        #:results '(#true #false))

      (test-refined-acl2 #:name 'cross-module-literals
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'module-with-literals)
           (same same)
           (same diff)])
        #:results '(#true #false)
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "module-with-literals"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))]))

    (test-suite "cross-module primitives"

      (test-refined-acl2 #:name 'primitive-provider
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require (only racket/base boolean?))
           (define-values {}
             (begin0 (#%plain-app values)
               (#%plain-lambda {} '#:primitive boolean? '1 'ACL2 'BOOLEANP)))
           (#%provide boolean?)])
        #:proof
        `[(,ASSERT-EVENT
            (,IF
              (,QUOTE ,T)
              (,QUOTE ,T)
              (,BOOLEANP
                (,QUOTE 1))))])

      (test-refined-acl2 #:name 'primitive-requirer
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'primitive-provider)
           (define-values {f}
             (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals
               (#%plain-app boolean? x)))])
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "primitive-provider"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,DEFUN F (X)
            (,BOOLEANP X))]))

    (test-suite "diamond modules"

      (test-refined-acl2 #:name 'DiamondA
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-values {A}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals 1))
           (#%provide A)])
        #:exports '[A]
        #:proof
        `[(,DEFUN A () (,QUOTE 1))])

      (test-refined-acl2 #:name 'DiamondB
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'DiamondA)
           (define-values {B}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (A)))
           (#%provide B)])
        #:exports '[B]
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "DiamondA"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,SKIP-PROOFS
            (,DEFUN A () (,QUOTE 1)))
          (,DEFUN B () (A))])

      (test-refined-acl2 #:name 'DiamondC
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'DiamondA)
           (define-values {C}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (A)))
           (#%provide C)])
        #:exports '[C]
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "DiamondA"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,SKIP-PROOFS
            (,DEFUN A () (,QUOTE 1)))
          (,DEFUN C () (A))])

      (test-refined-acl2 #:name 'DiamondD
        #:lang 'refined-acl2/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(#%require 'DiamondB 'DiamondC)
           (define-values {D1}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (B)))
           (define-values {D2}
             (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (C)))])
        #:proof
        `[(,MUST-SUCCEED
            (,INCLUDE-BOOK "DiamondB"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,MUST-SUCCEED
            (,INCLUDE-BOOK "DiamondC"
              #:SKIP-PROOFS-OKP ,T
              #:DIR #:DYNAMIC))
          (,SKIP-PROOFS
            (,DEFUN A () (,QUOTE 1)))
          (,SKIP-PROOFS (,DEFUN B () (A)))
          (,SKIP-PROOFS (,DEFUN C () (A)))
          (,DEFUN D1 () (B))
          (,DEFUN D2 () (C))]))))
