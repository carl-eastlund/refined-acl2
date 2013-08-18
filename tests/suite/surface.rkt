#lang mischief

(provide surface-tests)

(require
  rackunit
  mischief/preserve-expensive-metadata
  refined-acl2/tests/harness)

(define surface-tests
  (test-suite "surface syntax"

    (test-refined-acl2 #:name 'prelude-only
      #:lang 'refined-acl2/base
      #:proof prelude-proof)

    (test-refined-acl2 #:name 'export-and-evaluate
      #:lang 'refined-acl2/base
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define x "ecks")
         x
         (provide (all-defined-out))])
      #:results (list "ecks")
      #:exports '[x]
      #:checks
      (lambda {x}
        (check-equal? x "ecks"))
      #:proof
      `[,@prelude-proof
        (,DEFUN X () (,QUOTE "ecks"))])

    (test-refined-acl2 #:name 'recursive-list-length
      #:lang 'refined-acl2/base
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define (len x)
           (cond
             [(equal? x '()) 0]
             [(cons? x) (add1 (len (rest x)))]))])
      #:proof
      `[,@prelude-proof
        (,DEFUN LEN.2 (X)
          (,IF (,EQUAL X (,QUOTE ,EMPTY))
            (,QUOTE 0)
            (,IF (,CONSP X)
              (,1+ (LEN.2 (,REST X)))
              (,ILLEGAL
                (,QUOTE cond)
                (,QUOTE "all clauses failed")
                (,QUOTE ,EMPTY)))))])

    (test-refined-acl2 #:name 'macro-in-description
      #:lang 'refined-acl2/base
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(description D
           (~define (one x))
           (~define (both x y))
           (shorthand all
             [(_ x) (one x)]
             [(_ x . ys) (both (one x) (all . ys))]))
         (component C #:> D
           (define (one x) (- x))
           (define (both x y) (+ x y)))
         (use C)
         (C.all 1 2 3)])
      #:results '[-6]
      #:proof
      `[,@prelude-proof
        (,MUST-SUCCEED
          (,PROGN
            (,DEFUN C.ONE (X) (,UNARY-- X))
            (,DEFUN C.BOTH (X Y) (,BINARY-+ X Y))))
        (,DEFSTUB C.ONE (,*) ,=> ,*)
        (,DEFSTUB C.BOTH (,* ,*) ,=> ,*)])

    (test-refined-acl2 #:name 'macro-from-generic
      #:lang 'refined-acl2/base
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(description ADD
           (~define (add x y)))
         (description SUM
           (~component Add #:> ADD)
           (shorthand sum
             [(_ x) x]
             [(_ x . ys) ((dot Add add) x (sum . ys))]))
         (generic (Sum A #:> ADD) #:> SUM #:where {[Add #:= A]})
         (component Add-Number #:> ADD
           (define (add x y) (+ x y)))
         (instance Sum-Number (Sum Add-Number))
         (use Sum-Number)
         (define six (Sum-Number.sum 1 2 3))
         six])
      #:results '[6]
      #:proof
      `[,@prelude-proof
        (,MUST-SUCCEED
          (,DEFSTUB SUM.A.ADD (,* ,*) ,=> ,*))
        (,MUST-SUCCEED
          (,DEFUN ADD-NUMBER.ADD (X Y) (,BINARY-+ X Y)))
        (,DEFSTUB ADD-NUMBER.ADD (,* ,*) ,=> ,*)
        (,DEFUN SIX ()
          (ADD-NUMBER.ADD (,QUOTE 1)
            (ADD-NUMBER.ADD (,QUOTE 2) (,QUOTE 3))))])

    (test-refined-acl2 #:name 'binary-generic
      #:lang 'refined-acl2/base
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(description UNARY
           (~define (op x)))
         (generic (Compose [F #:> UNARY] [G #:> UNARY]) #:> UNARY
           (define (op x)
             ((dot F op) ((dot G op) x))))
         (component Identity #:> UNARY
           (define (op x) x))
         (instance Idempotent
           (Compose Identity Identity))
         ((dot Idempotent op) 7)])
      #:results '[7]
      #:proof
      `[,@prelude-proof
        (,MUST-SUCCEED
          (,PROGN
            (,DEFSTUB COMPOSE.F.OP (,*) ,=> ,*)
            (,DEFSTUB COMPOSE.G.OP (,*) ,=> ,*)
            (,MUST-SUCCEED
              (,DEFUN COMPOSE.OP (X) (COMPOSE.F.OP (COMPOSE.G.OP X))))
            (,DEFSTUB COMPOSE.OP (,*) ,=> ,*)))
        (,MUST-SUCCEED
          (,DEFUN IDENTITY.OP (X) X))
        (,DEFSTUB IDENTITY.OP (,*) ,=> ,*)
        (,DEFSTUB IDEMPOTENT.OP (,*) ,=> ,*)])

    (test-refined-acl2 #:name 'generics-with-where-clauses
      #:lang 'refined-acl2/base
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(description ONE (~define x))
         (description TWO (~component One #:> ONE))
         (description EMPTY)
         (generic (F [O #:> ONE] [T #:> TWO #:where {[One #:= O]}]) #:> EMPTY)
         (generic (G [O #:> ONE] [T #:> TWO #:where {[One #:= O]}]) #:> EMPTY
           (instance I (F O T)))])
      #:proof
      `[,@prelude-proof
        (,MUST-SUCCEED
          (,DEFSTUB F.O.X () ,=> ,*))
        (,MUST-SUCCEED
          (,DEFSTUB G.O.X () ,=> ,*))])))
