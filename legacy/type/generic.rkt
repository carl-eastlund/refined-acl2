#lang mischief

(require
  refined-acl2/legacy/type/base
  refined-acl2/legacy/type/description)

(struct generic-type type [domain formal range] #:transparent)

(provide
  (contract-out
    (struct {generic-type type}
      ([ref ref?]
       [domain description-type?]
       [formal identifier?]
       [range description-type?]))))
