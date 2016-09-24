(import (scheme case-lambda))
(define make-eq-hashtable
  (case-lambda
    (() (make-hashtable hash-function equiv))

(define (hashtable-update! hashtable key proc default)
        (hashtable-set! hashtable key (proc (hashtable-ref hashtable key default))))