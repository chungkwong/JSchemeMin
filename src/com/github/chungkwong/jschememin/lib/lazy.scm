(define-library (scheme lazy)
  (import (scheme base))
  (export delay-force delay make-promise force promise?)
  (begin
    (define make-promise-aux
      (lambda (done? proc)
        (list (cons done? proc))))

    (define-syntax delay-force
      (syntax-rules ()
        ((delay-force expression)
         (make-promise-aux #f (lambda () expression)))))

    (define-syntax delay
      (syntax-rules ()
        ((delay expression)
         (delay-force (make-promise-aux #t expression)))))

    (define make-promise
      (lambda (obj)
        (list (cons #t obj) obj)))

    (define (force promise)
      (if (promise-done? promise)
          (promise-value promise)
          (let ((promise* ((promise-value promise))))
            (unless (promise-done? promise)
              (promise-update! promise* promise))
            (force promise))))

    (define promise?
      (lambda (x) (and (pair? x) (pair? (car x)))))

    (define promise-done?
      (lambda (x) (car (car x))))
    (define promise-value
      (lambda (x) (cdr (car x))))
    (define promise-update!
      (lambda (new old)
        (set-car! (car old) (promise-done? new))
        (set-cdr! (car old) (promise-value new))
        (set-car! new (car old))))))