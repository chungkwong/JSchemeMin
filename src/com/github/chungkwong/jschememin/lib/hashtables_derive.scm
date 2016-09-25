(import (scheme base))
(import (scheme case-lambda))
(define make-eq-hashtable
  (case-lambda
    (() (make-hashtable equal-hash eq?))
    ((k) (make-hashtable equal-hash eq? k))))
(define make-eqv-hashtable
  (case-lambda
    (() (make-hashtable equal-hash eqv?))
    ((k) (make-hashtable equal-hash eq? k))))
(define (hashtable-update! hashtable key proc default)
        (hashtable-set! hashtable key (proc (hashtable-ref hashtable key default))))