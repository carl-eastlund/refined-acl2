#lang mischief

(require
  refined-acl2/legacy/type/base)

(struct function-type type [arity primitive] #:transparent)

(provide
  (contract-out
    (struct {function-type type}
      ([ref ref?]
       [arity nat/c]
       [primitive any/c]))))
