#lang mischief

(require
  refined-acl2/legacy/type/base
  refined-acl2/legacy/type/description)

(struct component-type type [description field~>type] #:transparent)

(provide
  (contract-out
    (struct {component-type type}
      ([ref ref?]
       [description description-type?]
       [field~>type (dict/c identifier? type?)]))))
