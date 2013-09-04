#lang setup/infotab

(define name "Dracula")

(define raco-commands
  (list
    (list
      "certify"
      '(submod refined-acl2/certify main)
      "certify Dracula modules using ACL2"
      #false)))

(define compile-omit-paths '("legacy" "examples"))

(define collection "refined-acl2")

(define deps '[mischief])
