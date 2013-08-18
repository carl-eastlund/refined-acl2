#lang mischief

(require
  refined-acl2/legacy/type/base
  refined-acl2/legacy/type/description)

(struct field-type type [field] #:transparent)

(provide
  (contract-out
    (struct {field-type type}
      ([ref ref?]
       [field identifier?]))))
