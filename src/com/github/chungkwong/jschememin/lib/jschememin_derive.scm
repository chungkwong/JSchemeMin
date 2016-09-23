(define-syntax duration (syntax-rules () ((_ x) (let ((t (thread-clock))) x (- (thread-clock) t)))))
(define-record-type <call-record> (make-call-record count total-time) call-record?
  (count count set-count!) (total-time total-time set-total-time!))
