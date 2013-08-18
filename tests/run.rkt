#lang mischief

(module+ main
  (require racket/cmdline)
  (define use-gui? #false)
  (command-line
    #:once-any
    ["--gui"
     "Use the RackUnit GUI."
     (set! use-gui? #true)]
    [{"--no-gui" "--text"}
     "Use the RackUnit textual interface (default)."
     (set! use-gui? #false)]
    #:args {}
    (if use-gui?
      ((dynamic-require 'rackunit/gui 'test/gui) #:wait? #true refined-acl2-tests)
      (exit ((dynamic-require 'rackunit/text-ui 'run-tests) refined-acl2-tests)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests refined-acl2-tests))

(require
  rackunit
  refined-acl2/tests/suite/atomic
  refined-acl2/tests/suite/modular
  refined-acl2/tests/suite/component
  refined-acl2/tests/suite/macro
  refined-acl2/tests/suite/surface)

(define refined-acl2-tests
  (test-suite "Dracula"
    surface-tests
    #|macro-tests|#
    component-tests
    modular-tests
    atomic-tests))
