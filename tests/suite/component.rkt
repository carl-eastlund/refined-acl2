#lang mischief

(provide component-tests)

(require
  rackunit
  mischief/preserve-expensive-metadata
  refined-acl2/tests/harness)

(define component-tests
  (test-suite "component system"

    (test-refined-acl2 #:name 'component-only
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {comp}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(f) (#%plain-lambda {x}
                         '#:function '#:no-measure '#:no-goals
                         x)]
                  [(x0) (#%plain-lambda {}
                          '#:function '#:no-measure '#:no-goals
                          '0)])
                 (#%plain-app values 'f f 'x0 x0)))))
         (define-values {y}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref comp 'f)
               (#%plain-app (#%plain-app #%deref comp 'x0)))))
         (#%provide y)])
      #:exports '(y)
      #:checks (lambda (y) (check-equal? (y) 0))
      #:proof
      `[(,DEFUN COMP.F (X) X)
        (,DEFUN COMP.X0 () (,QUOTE 0))
        (,DEFUN Y () (COMP.F (COMP.X0)))])

    (test-refined-acl2 #:name 'description-and-component
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {desc}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '1)]
                    [(x0) (#%plain-lambda {} '#:stub '0)])
                   (#%plain-app values 'f f 'x0 x0))))))
         (define-values {comp}
           (#%plain-app #%seal desc
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {x}
                           '#:function '#:no-measure '#:no-goals
                           x)]
                    [(x0) (#%plain-lambda {}
                            '#:function '#:no-measure '#:no-goals
                            '0)])
                   (#%plain-app values 'f f 'x0 x0))))))
         (define-values {y}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref comp 'f)
               (#%plain-app (#%plain-app #%deref comp 'x0)))))
         (#%provide y)])
      #:exports '(y)
      #:checks (lambda (y) (check-equal? (y) 0))
      #:proof
      `[(,MUST-SUCCEED
          (,PROGN
            (,DEFUN COMP.F (X) X)
            (,DEFUN COMP.X0 () (,QUOTE 0))))
        (,DEFSTUB COMP.F (,*)
          ,=> ,*)
        (,DEFSTUB COMP.X0 () ,=> ,*)
        (,DEFUN Y () (COMP.F (COMP.X0)))])

    (test-refined-acl2 #:name 'spliced-definitions
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {desc}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(a) (#%plain-lambda {} '#:stub '0)]
                    [(b)
                     (#%plain-lambda {} '#:function '#:no-measure
                       (#%plain-app c))]
                    [(c) (#%plain-lambda {} '#:stub '0)])
                   (#%plain-app values 'a a 'b b 'c c))))))
         (define-values {comp}
           (#%plain-app #%seal desc
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(a)
                     (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                       (#%plain-app b))]
                    [(b)
                     (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                       (#%plain-app c))]
                    [(c)
                     (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                       '0)])
                   (#%plain-app values 'c c 'b b 'a a))))))])
      #:proof
      `[(,MUST-SUCCEED
          (,PROGN
            (,DEFUN COMP.C () (,QUOTE 0))
            (,DEFUN COMP.B () (COMP.C))
            (,DEFUN COMP.A () (COMP.B))))
        (,DEFSTUB COMP.A () ,=> ,*)
        (,DEFSTUB COMP.C () ,=> ,*)
        (,SKIP-PROOFS (,DEFUN COMP.B () (COMP.C)))])

    (test-refined-acl2/syntax-error #:name 'spliced-recursion
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {desc}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(a) (#%plain-lambda {} '#:stub '0)]
                    [(b)
                     (#%plain-lambda {} '#:function '#:no-measure
                       (#%plain-app a))])
                   (#%plain-app values 'a a 'b b))))))
         (define-values {comp}
           (#%plain-app #%seal desc
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(b)
                     (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                       (#%plain-app a))]
                    [(a)
                     (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                       (#%plain-app b))])
                   (#%plain-app values 'a a 'b b))))))])
      #:error-message mutual-recursion-regexps)

    (test-refined-acl2/syntax-error #:name 'mutually-recursive-components
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {a}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(f)
                   (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                     (#%plain-app (#%plain-app #%deref b 'f)))])
                 (#%plain-app values 'f f)))))
         (define-values {b}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(f)
                   (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                     (#%plain-app (#%plain-app #%deref a 'f)))])
                 (#%plain-app values 'f f)))))])
      #:error-message mutual-recursion-regexps)

    (test-refined-acl2/syntax-error #:name 'mutually-recursive-generic/instance
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {g}
           (#%plain-lambda {}
             (begin0
               (#%plain-app #%instance
                 (#%plain-lambda {}
                   (letrec-syntaxes+values {}
                     ([(f)
                       (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
                         (#%plain-app (#%plain-app #%deref i 'f)))])
                     (#%plain-app values 'f f)))))))
         (define-values {i}
           (#%instantiate g))])
      #:error-message mutual-recursion-regexps)

    (test-refined-acl2/syntax-error #:name 'recursive-instance
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (#%plain-app values)))))
         (define-values {g}
           (#%plain-lambda {a}
             (begin0
               (#%plain-app #%instance
                 (#%plain-lambda {}
                   (#%plain-app values)))
               t)))
         (define-values {i}
           (#%plain-app #%instantiate g i))])
      #:error-message self-recursion-regexps)

    (test-refined-acl2/syntax-error #:name 'top-level-function-declaration
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {f}
           (#%plain-lambda {} '#:stub '0))])
      #:error-message decl-as-defn-regexps)

    (test-refined-acl2/syntax-error #:name 'bad-implementation-of-description
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '1)])
                   (#%plain-app values 'f f))))))
         (define-values {i}
           (#%plain-app #%seal t
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f)
                     (#%plain-lambda {x y}
                       '#:function '#:no-measure '#:no-goals
                       'result)])
                   (#%plain-app values 'f f))))))])
      #:error-message (wrong-arity-regexps #:expected 1 #:actual 2))

    (test-refined-acl2 #:name 'description-refinement
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '1)]
                    [(g)
                     (#%plain-lambda {x}
                       '#:function '#:no-measure
                       (#%plain-app f (#%plain-app f x)))])
                   (#%plain-app values 'f f 'g g))))))
         (define-values {ident}
           (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals x))
         (define-values {i}
           (#%plain-app #%seal
             (#%plain-lambda {} '#:refine t '[f] ident)
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) ident]
                    [(g)
                     (#%plain-lambda {x}
                       '#:function '#:no-measure '#:no-goals
                       (#%plain-app f (#%plain-app ident x)))])
                   (#%plain-app values 'f f 'g g))))))
         (define-values {x}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref i 'f) '7)))
         (define-values {y}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref i 'g) (#%plain-app x))))
         (#%provide ident x y)])
      #:exports '[ident x y]
      #:checks
      (lambda (ident x y)
        (check-equal? (ident 'value) 'value)
        (check-equal? (x) 7)
        (check-equal? (y) 7))
      #:proof
      `[(,DEFUN IDENT (X) X)
        (,MUST-SUCCEED
          (,DEFUN I.G (X)
            (IDENT (IDENT X))))
        (,SKIP-PROOFS
          (,DEFUN I.G (X)
            (IDENT (IDENT X))))
        (,DEFUN X ()
          (IDENT (,QUOTE 7)))
        (,DEFUN Y ()
          (I.G (X)))])

    (test-refined-acl2/syntax-error #:name 'bad-description-refinement
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '2)])
                   (#%plain-app values 'f f))))))
         (define-values {ident}
           (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals x))
         (define-values {i}
           (#%plain-app #%seal (#%plain-lambda {} '#:refine t '[f] ident)
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) ident])
                   (#%plain-app values 'f f))))))])
      #:error-message (wrong-arity-regexps #:expected 2 #:actual 1))

    (test-refined-acl2/syntax-error #:name 'label-for-external-definition
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {ident}
           (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals x))
         (define-values {i}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (#%plain-app values 'f ident))))])
      #:error-message
      '[#px"instance"
        #px"label|member|define|definition"
        #px"ident"])

    (test-refined-acl2 #:name 'nested-component
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {outer}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(inner)
                   (#%plain-app #%instance
                     (#%plain-lambda {}
                       (letrec-syntaxes+values {}
                         ([(f)
                           (#%plain-lambda {}
                             '#:function '#:no-measure '#:no-goals
                             '2)])
                         (#%plain-app values 'f f))))])
                 (#%plain-app values 'inner inner)))))
         (define-values {two}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app
               (#%plain-app #%deref
                 (#%plain-app #%deref outer 'inner)
                 'f))))
         (#%provide two)])
      #:exports '[two]
      #:checks
      (lambda (two)
        (check-equal? (two) 2))
      #:proof
      `[(,DEFUN OUTER.INNER.F () (,QUOTE 2))
        (,DEFUN TWO () (OUTER.INNER.F))])

    (test-refined-acl2 #:name 'nested-component-type
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {flat}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '0)])
                   (#%plain-app values 'f f))))))
         (define-values {nested}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(inner) flat])
                   (#%plain-app values 'inner inner))))))
         (define-values {outer}
           (#%plain-app #%seal nested
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(inner)
                     (#%plain-app #%seal flat
                       (#%plain-app #%instance
                         (#%plain-lambda {}
                           (letrec-syntaxes+values {}
                             ([(f)
                               (#%plain-lambda {}
                                 '#:function '#:no-measure '#:no-goals
                                 '1)])
                             (#%plain-app values 'f f)))))])
                   (#%plain-app values 'inner inner))))))
         (define-values {one}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app
               (#%plain-app #%deref
                 (#%plain-app #%deref outer 'inner)
                 'f))))
         (#%provide one)])
      #:exports '[one]
      #:checks
      (lambda (one)
        (check-equal? (one) 1))
      #:proof
      `[(,MUST-SUCCEED
          (,PROGN
            (,MUST-SUCCEED
              (,DEFUN OUTER.INNER.F ()
                (,QUOTE 1)))
            (,DEFSTUB OUTER.INNER.F ()
              ,=> ,*)))
        (,DEFSTUB OUTER.INNER.F () ,=> ,*)
        (,DEFUN ONE () (OUTER.INNER.F))])

    (test-refined-acl2 #:name 'description-in-description
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {outer}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(inner)
                     (#%plain-lambda {} '#:instance
                       (#%plain-lambda {}
                         (#%plain-app values)))])
                   (#%plain-app values 'inner inner))))))]))

    (test-refined-acl2 #:name 'description-in-description-in-description
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {outer}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(middle)
                     (#%plain-lambda {} '#:instance
                       (#%plain-lambda {}
                         (letrec-syntaxes+values {}
                           ([(inner)
                             (#%plain-lambda {} '#:instance
                               (#%plain-lambda {}
                                 (#%plain-app values)))])
                           (#%plain-app values 'inner inner))))])
                   (#%plain-app values 'middle middle))))))]))

    (test-refined-acl2 #:name 'refined-component
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {simple}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(f)
                   (#%plain-lambda {}
                     '#:function '#:no-measure '#:no-goals
                     '-1)])
                 (#%plain-app values 'f f)))))
         (define-values {nested}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(inner)
                     (#%plain-lambda {} '#:instance
                       (#%plain-lambda {}
                         (letrec-syntaxes+values {}
                           ([(f) (#%plain-lambda {} '#:stub '0)])
                           (#%plain-app values 'f f))))])
                   (#%plain-app values 'inner inner))))))
         (define-values {outer}
           (#%plain-app #%seal
             (#%plain-lambda {} '#:refine nested '[inner] simple)
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(inner) simple])
                   (#%plain-app values 'inner inner))))))
         (define-values {f}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app
               (#%plain-app #%deref
                 (#%plain-app #%deref outer 'inner)
                 'f))))
         (#%provide f)])
      #:exports '[f]
      #:checks
      (lambda (f)
        (check-equal? (f) -1))
      #:proof
      `[(,DEFUN SIMPLE.F () (,QUOTE -1))
        (,DEFUN F () (SIMPLE.F))])

    (test-refined-acl2 #:name 'simple-generic
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {domain}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '0)])
                   (#%plain-app values 'f f))))))
         (define-values {input->output}
           (#%plain-lambda {input}
             (begin0
               (#%plain-app #%instance
                 (#%plain-lambda {}
                   (letrec-syntaxes+values {}
                     ([(g)
                       (#%plain-lambda {}
                         '#:function '#:no-measure '#:no-goals
                         (if (#%plain-app (#%plain-app #%deref input 'f))
                           'yes
                           'no))])
                     (#%plain-app values 'g g))))
               domain)))
         (define-values {input}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(f)
                   (#%plain-lambda {}
                     '#:function '#:no-measure '#:no-goals
                     '#true)])
                 (#%plain-app values 'f f)))))
         (define-values {output}
           (#%plain-app #%instantiate input->output input))
         (#%plain-app (#%plain-app #%deref output 'g))])
      #:results (list 'yes)
      #:proof
      `[(,MUST-SUCCEED
          (,PROGN
            (,DEFSTUB INPUT->OUTPUT.INPUT.F ()
              ,=> ,*)
            (,DEFUN INPUT->OUTPUT.G ()
              (,IF (INPUT->OUTPUT.INPUT.F)
                (,QUOTE yes)
                (,QUOTE no)))))
        (,DEFUN INPUT.F () (,QUOTE ,T))
        (,SKIP-PROOFS
          (,DEFUN OUTPUT.G ()
            (,IF (INPUT.F)
              (,QUOTE yes)
              (,QUOTE no))))])

    (test-refined-acl2 #:name 'generic-with-refinement
      #:lang 'refined-acl2/kernel

      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {x/y}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(x) (#%plain-lambda {} '#:stub '0)]
                    [(y) (#%plain-lambda {} '#:stub '0)])
                   (#%plain-app values 'x x 'y y))))))
         (define-values {toggle}
           (#%plain-lambda {input}
             (begin0
               (#%plain-app #%seal
                 (#%plain-lambda {} '#:refine x/y '[x]
                   (#%plain-app #%deref input 'x))
                 (#%plain-app #%instance
                   (#%plain-lambda {}
                     (letrec-syntaxes+values {}
                       ([(x) (#%plain-app #%deref input 'x)]
                        [(y)
                         (#%plain-lambda {}
                           '#:function '#:no-measure '#:no-goals
                           (if (#%plain-app (#%plain-app #%deref input 'y))
                             '#false
                             (#%plain-app x)))])
                       (#%plain-app values 'x x 'y y)))))
               x/y)))
         (define-values {five/six}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(x)
                   (#%plain-lambda {}
                     '#:function '#:no-measure '#:no-goals
                     '5)]
                  [(y)
                   (#%plain-lambda {}
                     '#:function '#:no-measure '#:no-goals
                     '6)])
                 (#%plain-app values 'x x 'y y)))))
         (define-values {flip}
           (#%plain-app #%instantiate toggle five/six))
         (define-values {flop}
           (#%plain-app #%instantiate toggle flip))
         (define-values {funf}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref flip 'x))))
         (define-values {false}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref flip 'y))))
         (define-values {finnif}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref flop 'x))))
         (define-values {finally}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref flop 'y))))
         (#%provide funf false finnif finally)])

      #:exports '[funf false finnif finally]
      #:checks
      (lambda (funf false finnif finally)
        (check-equal? (funf) 5)
        (check-equal? (false) #false)
        (check-equal? (finnif) 5)
        (check-equal? (finally) 5))

      #:proof
      `[(,MUST-SUCCEED
          (,PROGN
            (,DEFSTUB TOGGLE.INPUT.X () ,=> ,*)
            (,DEFSTUB TOGGLE.INPUT.Y () ,=> ,*)
            (,MUST-SUCCEED
              (,DEFUN TOGGLE.Y ()
                (,IF (TOGGLE.INPUT.Y)
                  (,QUOTE ,NIL)
                  (TOGGLE.INPUT.X))))
            (,DEFSTUB TOGGLE.Y () ,=> ,*)))
        (,DEFUN FIVE/SIX.X () (,QUOTE 5))
        (,DEFUN FIVE/SIX.Y () (,QUOTE 6))
        (,DEFSTUB FLIP.Y () ,=> ,*)
        (,DEFSTUB FLOP.Y () ,=> ,*)
        (,DEFUN FUNF () (FIVE/SIX.X))
        (,DEFUN FALSE () (FLIP.Y))
        (,DEFUN FINNIF () (FIVE/SIX.X))
        (,DEFUN FINALLY () (FLOP.Y))])

    (test-refined-acl2 #:name 'generic-declaration
      #:lang 'refined-acl2/kernel

      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {flat}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(f) (#%plain-lambda {} '#:stub '0)]
                    [(g) (#%plain-lambda {} '#:stub '0)])
                   (#%plain-app values 'f f 'g g))))))
         (define-values {deep}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(gen)
                     (#%plain-lambda {formal}
                       (begin0
                         (#%plain-lambda {} '#:refine flat '[f]
                           (#%plain-app #%deref formal 'f))
                         flat))])
                   (#%plain-app values 'gen gen))))))
         (define-values {one}
           (#%plain-app #%seal deep
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(gen)
                     (#%plain-lambda {input}
                       (begin0
                         (#%plain-app #%instance
                           (#%plain-lambda {}
                             (letrec-syntaxes+values {}
                               ([(f) (#%plain-app #%deref input 'f)]
                                [(g) (#%plain-app #%deref input 'g)])
                               (#%plain-app values 'f f 'g g))))
                         flat))])
                   (#%plain-app values 'gen gen))))))
         (define-values {two}
           (#%plain-app #%seal
             (#%plain-lambda {} '#:refine deep '[gen]
               (#%plain-app #%deref one 'gen))
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(gen) (#%plain-app #%deref one 'gen)])
                   (#%plain-app values 'gen gen))))))
         (define-values {zero}
           (#%plain-app #%instance
             (#%plain-lambda {}
               (letrec-syntaxes+values {}
                 ([(f)
                   (#%plain-lambda {}
                     '#:function '#:no-measure '#:no-goals
                     'eff)]
                  [(g)
                   (#%plain-lambda {}
                     '#:function '#:no-measure '#:no-goals
                     'gee)])
                 (#%plain-app values 'f f 'g g)))))
         (define-values {two*zero}
           (#%plain-app #%instantiate (#%plain-app #%deref two 'gen) zero))
         (define-values {f}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref two*zero 'f))))
         (define-values {g}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app (#%plain-app #%deref two*zero 'g))))
         (#%provide f g)])

      #:exports '(f g)
      #:checks
      (lambda (f g)
        (check-equal? (f) 'eff)
        (check-equal? (g) 'gee))

      #:proof
      `[(,MUST-SUCCEED
          (,MUST-SUCCEED
            (,PROGN
              (,DEFSTUB ONE.GEN.INPUT.F ()
                ,=> ,*)
              (,DEFSTUB ONE.GEN.INPUT.G ()
                ,=> ,*))))
        (,DEFUN ZERO.F () (,QUOTE eff))
        (,DEFUN ZERO.G () (,QUOTE gee))
        (,DEFSTUB TWO*ZERO.G () ,=> ,*)
        (,DEFUN F () (ZERO.F))
        (,DEFUN G () (TWO*ZERO.G))])

    (test-refined-acl2 #:name 'theorem-declaration
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(claim)
                     (#%plain-lambda {x}
                       '#:theorem
                       (#%plain-lambda {} '#:rules
                         (#%plain-lambda {} '#:rewrite '#:no-corollary))
                       (if x '#true '#true))])
                   (#%plain-app values 'claim claim))))))
         (define-values {i}
           (#%plain-app #%seal t
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(claim)
                     (#%plain-lambda {x}
                       '#:theorem
                       (#%plain-lambda {} '#:rules
                         (#%plain-lambda {} '#:rewrite '#:no-corollary))
                       (#%plain-lambda {} '#:goals
                         (#%plain-lambda {} '#:goal '"Goal"
                           (#%plain-lambda {} '#:by (#%plain-app lemma x))))
                       (if x '#true '#true))]
                    [(lemma)
                     (#%plain-lambda {x}
                       '#:theorem
                       '#:no-rules
                       '#:no-goals
                       '#true)])
                   (#%plain-app values 'claim claim 'lemma lemma))))))])
      #:proof
      `[(,MUST-SUCCEED
          (,PROGN
            (,DEFTHM I.LEMMA
              (,QUOTE ,T))
            (,DEFTHM I.CLAIM
              (,IF X
                (,QUOTE ,T)
                (,QUOTE ,T))
              #:RULE-CLASSES
              ((#:REWRITE))
              #:HINTS
              (("Goal" #:BY (#:INSTANCE I.LEMMA (X X)))))))
        (,SKIP-PROOFS
          (,DEFTHM I.CLAIM
            (,IF X
              (,QUOTE ,T)
              (,QUOTE ,T))
            #:RULE-CLASSES
            ((#:REWRITE))))])

    (test-refined-acl2/syntax-error #:name 'theorem-rule-class-mismatch
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(claim)
                     (#%plain-lambda {x}
                       '#:theorem
                       (#%plain-lambda {} '#:rules
                         (#%plain-lambda {} '#:rewrite '#:no-corollary))
                       (if x '#true '#true))])
                   (#%plain-app values 'claim claim))))))
         (define-values {i}
           (#%plain-app #%seal t
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(claim)
                     (#%plain-lambda {x}
                       '#:theorem
                       (#%plain-lambda {} '#:rules
                         (#%plain-lambda {} '#:elim '#:no-corollary))
                       '#:no-goals
                       (if x '#true '#true))])
                   (#%plain-app values 'claim claim))))))])
      #:error-message
      '[#px"rule class"])

    (test-refined-acl2/syntax-error #:name 'theorem-body-mismatch
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {t}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(claim)
                     (#%plain-lambda {x}
                       '#:theorem
                       '#:no-rules
                       (if x '#true '#true))])
                   (#%plain-app values 'claim claim))))))
         (define-values {i}
           (#%plain-app #%seal t
             (#%plain-app #%instance
               (#%plain-lambda {}
                 (letrec-syntaxes+values {}
                   ([(claim)
                     (#%plain-lambda {x}
                       '#:theorem
                       '#:no-rules
                       '#:no-goals
                       '#true)])
                   (#%plain-app values 'claim claim))))))])
      #:error-message
      '[#px"theorem"
        #px"body|bodies"
        #px"mismatch"])

    (test-refined-acl2 #:name 'generics-with-where-clauses
      #:lang 'refined-acl2/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {ONE}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-values {[{x} (#%plain-lambda {} '#:stub '0)]}
                   (#%plain-app values 'x x))))))
         (define-values {TWO}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (letrec-values {[(One) ONE]}
                   (#%plain-app values 'One One))))))
         (define-values {EMPTY}
           (#%plain-lambda {} '#:type
             (#%plain-lambda {} '#:instance
               (#%plain-lambda {}
                 (#%plain-app values)))))
         (define-values {F}
           (#%plain-lambda {O T}
             (begin0
               (#%plain-app #%seal EMPTY
                 (#%plain-app #%instance
                   (#%plain-lambda {}
                     (#%plain-app values))))
               ONE
               (#%plain-lambda {} '#:refine TWO '(One) O))))
         (define-values {G}
           (#%plain-lambda {O T}
             (begin0
               (#%plain-app #%seal EMPTY
                 (#%plain-app #%instance
                   (#%plain-lambda {}
                     (letrec-values {[(I) (#%plain-app #%instantiate F O T)]}
                       (#%plain-app values 'I I)))))
               ONE
               (#%plain-lambda {} '#:refine TWO '(One) O))))])
      #:proof
      `[(,MUST-SUCCEED
          (,DEFSTUB F.O.X () ,=> ,*))
        (,MUST-SUCCEED
          (,DEFSTUB G.O.X () ,=> ,*))])))
