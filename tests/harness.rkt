#lang mischief

(provide
  (all-defined-out))

(require
  (for-syntax
    mischief)
  rackunit
  racket/sandbox
  refined-acl2/expansion/paths
  "diff.rkt")

(define mutual-recursion-regexps
  '[#px"cycle|(mutual.+(recursion|recursive))"])

(define self-recursion-regexps
  '[#px"undefined|unrecognized|cannot find type"])

(define decl-as-defn-regexps
  '[#px"define|definition|term" #px"declare|declaration|type"])

(define (wrong-arity-regexps #:expected exp #:actual act)
  (list
    '#px"arity|argument"
    (regexp-quote (count->phrase exp "argument"))
    (regexp-quote (count->phrase act "argument"))))

(define (ACL2 sym) (prefab ':: 'ACL2 sym))

(define-shorthand (define-ACL2 NAME:id ...)
  (define-values {NAME ...} (values (ACL2 'NAME) ...)))

(define-ACL2
  DEFUN DEFTHM DEFSTUB
  DEFLABEL
  ASSERT-EVENT
  INCLUDE-BOOK
  MUST-SUCCEED PROGN
  ENCAPSULATE LOCAL * =>
  SKIP-PROOFS
  DECLARE XARGS
  IN-THEORY ENABLE DISABLE
  IF LET QUOTE T NIL
  EQUAL
  EMPTY EMPTY? VOID VOID?
  CONSP CONS CAR CDR FIRST REST
  BOOLEANP
  BINARY-APPEND
  STRINGP COERCE
  CHARACTERP CHAR-CODE CODE-CHAR
  ACL2-NUMBER RATIONALP INTEGERP
  BINARY-+ UNARY-- BINARY-* UNARY-/ < =
  1+ 1- ZP ZIP ZEROP
  ILLEGAL)

(define prelude-book-name
  (book-path-without-extension
    (resolved-module-path-name
      (resolve-module-path
        'refined-acl2/base))))

(define prelude-proof
  `[(,MUST-SUCCEED
      (,INCLUDE-BOOK ,prelude-book-name
        #:SKIP-PROOFS-OKP ,T))])

(define (test-refined-acl2
          #:name [name (gensym 'anonymous-module)]
          #:lang [lang 'refined-acl2]
          #:program [body-stxs #'()]
          #:namespace [namespace refined-acl2-namespace]
          #:exports [exports '()]
          #:proof [proof '()]
          #:results [results '()]
          #:checks [checks void])
  (test-program
    #:name name
    #:lang lang
    #:program body-stxs
    #:namespace namespace
    #:compile (arg+ check-refined-acl2-compile
                #:exports exports #:proof proof
                #:execute (arg+ check-refined-acl2-execute
                            #:results results
                            #:exports exports
                            #:checks checks))))

(define (test-refined-acl2/expansion/runtime-error
          #:name [name (gensym 'anonymous-module)]
          #:lang [lang 'refined-acl2/kernel]
          #:program [body-stxs #'()]
          #:namespace [namespace refined-acl2-namespace]
          #:exports [exports '()]
          #:proof [proof '()]
          #:error-message [error-message '()])
  (test-program
    #:name name
    #:lang lang
    #:program body-stxs
    #:namespace namespace
    #:compile (arg+ check-refined-acl2-compile
                #:exports exports #:proof proof
                #:execute (arg+ check-refined-acl2-runtime-error
                            #:error-message error-message))))

(define (test-refined-acl2/syntax-error
          #:name [name (gensym 'anonymous-module)]
          #:lang [lang 'refined-acl2/kernel]
          #:program [body-stxs #'()]
          #:namespace [namespace refined-acl2-namespace]
          #:error-message [error-message '()])
  (test-program
    #:name name
    #:lang lang
    #:program body-stxs
    #:namespace namespace
    #:compile (arg+ check-refined-acl2-syntax-error
                #:error-message error-message)))

(define (check-refined-acl2-compile name stx
          #:exports [exports '()]
          #:proof [proof '()]
          #:execute [execute check-refined-acl2-execute])
  (check-not-exn
    (lambda ()
      (eval stx)))
  (check-refined-acl2-exports name exports)
  (check-refined-acl2-proof name proof)
  (execute name))

(define (check-refined-acl2-exports name expected)
  (check-not-exn
    (lambda ()
      (define actual
        (dynamic-quote-require
          (list 'quote name)))
      (define actual-set
        (list->seteq actual))
      (with-check-info {['actual-exports actual]
                        ['expected-exports expected]}
        (for {[sym (in-list expected)]
              #:unless (set-member? actual-set sym)}
          (fail (format "missing export: ~s" sym)))))))

(define (check-refined-acl2-proof name expected)
  (check-not-exn
    (lambda ()
      (define actual
        (dynamic-refined-acl2-proof
          (list 'quote name)))
      (define difference
        (diff expected actual))
      (with-check-info {['actual-proof actual]
                        ['expected-proof expected]
                        ['proof-diff difference]}
        (for/first {[part (in-list difference)]}
          (fail (format "first difference: ~v" part)))))))

(define (check-refined-acl2-syntax-error name stx
          #:error-message [error-message '()])
  (define (error? x)
    (and (exn:fail:syntax? x)
      (for/and {[rx (in-list error-message)]}
        (regexp-match? rx (exn-message x)))))
  (check-exn error?
    (lambda ()
      (eval stx))))

(define (check-refined-acl2-execute name
          #:results [results '()]
          #:exports [exports '()]
          #:checks [checks void])
  (define port
    (open-output-string))
  (check-not-exn
    (lambda ()
      (parameterize {[current-output-port port]}
        (dynamic-require (list 'quote name) #false))))
  (check-refined-acl2-results port results)
  (apply checks
    (for/list {[sym (in-list exports)]}
      (dynamic-require (list 'quote name) sym))))

(define (check-refined-acl2-results port results)
  (define actual (get-output-string port))
  (define expected
    (with-output-to-string
      (lambda ()
        (for {[result (in-list results)]}
          ((current-print) result)))))
  (with-check-info {['expected-output expected]
                    ['actual-output actual]}
    (check-equal? actual expected)))

(define (check-refined-acl2-runtime-error name
          #:error-message [error-message '()])
  (define (error? x)
    (and (exn:fail? x)
      (for/and {[rx (in-list error-message)]}
        (regexp-match? rx (exn-message x)))))
  (check-exn error?
    (lambda ()
      (dynamic-require (list 'quote name) #false))))

(define (test-program
          #:name [name (gensym 'anonymous-module)]
          #:lang [lang 'racket]
          #:program [body-stxs #'()]
          #:namespace [namespace (current-namespace)]
          #:module-begin? [module-begin? #true]
          #:compile [compile check-compile]
          #:time-limit-seconds [time-limit 60]
          #:memory-limit-megabytes [memory-limit 1024])
  (test-case (symbol->string name)
    (define stx
      #`(module #,name #,lang
          #,@(strip-context
               (if module-begin?
                 (list #`(#%module-begin #,@body-stxs))
                 body-stxs))))
    (define sandbox
      (call-with-trusted-sandbox-configuration
        (lambda ()
          (parameterize {[sandbox-eval-limits (list time-limit memory-limit)]}
            (make-evaluator 'racket/base)))))
    (define guard (current-security-guard))
    (call-in-sandbox-context sandbox
      (lambda ()
        (parameterize {[current-namespace namespace]}
          (with-check-info {['program (to-datum stx)]}
            (compile name stx)))))))

(define (check-compile name stx #:execute [execute check-execute])
  (check-not-exn
    (lambda ()
      (eval stx)))
  (execute name))

(define (check-execute name)
  (check-not-exn
    (lambda ()
      (dynamic-require (list 'quote name) #false))))

(define refined-acl2-namespace
  (make-base-namespace))

(define (dynamic-quote-require mod-path [namespace (current-namespace)])
  (parameterize {[current-namespace namespace]}
    (define sym (gensym))
    (eval
      `(,#'module ,sym racket/base
         (#%module-begin
           (provide ,sym)
           (require mischief/require)
           (define ,sym (quote-require ,mod-path)))))
    (dynamic-require (list 'quote sym) sym)))

(define (dynamic-refined-acl2-proof mod-path [namespace (current-namespace)])
  (parameterize {[current-namespace namespace]}
    ((dynamic-require
       'refined-acl2/proof/dynamic
       'module-path->proof-obligation)
     mod-path)))
