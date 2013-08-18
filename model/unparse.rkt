#lang mischief

(provide
  make-refine-expr)

(require
  (for-template
    refined-acl2/kernel))

(define (make-refine-expr base-stx field-ids defn-stxs)
  (for/fold
      {[stx base-stx]}
      {[field-id (in-list field-ids)]
       [defn-stx (in-list defn-stxs)]}
    #`(#%plain-lambda {} '#:refine #,stx '[#,field-id] #,defn-stx)))
