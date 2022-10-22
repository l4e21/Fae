;;;; package.lisp

(defpackage :fae
  (:use #:cl)
  (:export #:range
           #:fuzzy-set
           #:fuzzy-cons
           #:fuzzy-cons-weight
           #:fuzzy-set-p
           #:crisp-members
           #:fuzzy-members
           #:strong-alpha-cut
           #:alpha-cut
           #:fuzzy-support
           #:fuzzy-and
           #:fuzzy-or
           #:compl
           #:fuzzy-intersection
           #:fuzzy-union
           #:height
           #:fuzzy-subset?
           #:scalar-cardinality
           #:relative-cardinality
           #:fuzzy-cardinality
           #:simple-difference
           #:simple-disjoint-sum
           #:null-mu
           #:fuzzy-minus
           #:bounded-difference
           #:minkowski-distance
           #:hamming-distance
           #:euler-distance
           #:fuzzy-analysis
           #:ff
           #:keys
           #:make-table
           #:make-db
           #:attribute
           #:table-lookup
           #:store-domain
           #:load-into-domain
           #:init-env
           #:make-env
           #:make-result
           #:interpret-rule
           #:evaluate-rule
           #:fae
           #:init!))
