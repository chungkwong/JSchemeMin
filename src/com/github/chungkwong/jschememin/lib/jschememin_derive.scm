(import (scheme base))
(import (scheme hashtables))
(define duration (syntax-rules () ((_ x) (let ((t (thread-clock))) x (- (thread-clock) t)))))
(define-record-type <call-record> (make-call-record count total-time) call-record?
  (count count set-count!) (total-time total-time set-total-time!))
(define statistics (make-eq-hashtable))
(define profile-lambda
  (syntax-rules ()
    ((_ args body ...)
     (letrec ((proc (lambda args
                             (let* ((t (thread-clock)) (ret (begin body ... )))
                                   (hashtable-update! statistics
                                                      proc
                                                      (lambda (e) (set-total-time! e (+ (total-time e) (- (thread-clock) t)))
                                                                  (set-count! e (+ (count e) 1))
                                                                  e)
                                                      (make-call-record 0 0))
                                   ret))))
              proc))))
(define (profile-record proc) (hashtable-ref statistics proc (make-call-record 0 0)))
(define (profile-records) (hashtable-copy statistics #f))