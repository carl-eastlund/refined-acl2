#lang mischief

(require
  refined-acl2/legacy/type/base)

(struct value-type type [] #:transparent)

(provide
  (contract-out
    (struct {value-type type} ([ref ref?]))))
