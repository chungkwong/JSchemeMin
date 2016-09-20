(define define-syntax define)

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))



(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
     (letrec "generate_temp_names"
       (var1 ...)
       ()
       ((var1 init1) ...)
       body ...))
    ((letrec "generate_temp_names"
       ()
       (temp1 ...)
       ((var1 init1) ...)
       body ...)
     (let ((var1 '<undefined>) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1)
         ...
         body ...)))
    ((letrec "generate_temp_names"
       (x y ...)
       (temp ...)
       ((var1 init1) ...)
       body ...)
     (letrec "generate_temp_names"
       (y ...)
       (newtemp temp ...)
       ((var1 init1) ...)
       body ...))))



(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 '<undefined>) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))



(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind"
         (binding ...) () (begin body0 body1 ...)))

    ((let-values "bind" () tmps body)
     (let tmps body))

    ((let-values "bind" ((b0 e0)
         binding ...) tmps body)
     (let-values "mktmp" b0 e0 ()
         (binding ...) tmps body))

    ((let-values "mktmp" () e0 args
         bindings tmps body)
     (call-with-values
       (lambda () e0)
       (lambda args
         (let-values "bind"
             bindings tmps body))))

    ((let-values "mktmp" (a . b) e0 (arg ...)
         bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x)
         bindings (tmp ... (a x)) body))

    ((let-values "mktmp" a e0 (arg ...)
        bindings (tmp ...) body)
     (call-with-values
       (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind"
             bindings (tmp ... (a x)) body))))))



(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (let () body0 body1 ...))

    ((let*-values (binding0 binding1 ...)
         body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
         body0 body1 ...)))))

(define let-syntax let)
(define letrec-syntax letrec)

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))



(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))



(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))



(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))



(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))



(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))





(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

(define (make-parameter init . o)
  (let* ((converter
          (if (pair? o) (car o) (lambda (x) x)))
         (value (converter init)))
    (lambda args
      (cond
       ((null? args)
        value)
       ((equal? (car args) "<param-set!>")
        (set! value (cadr args)))
       ((equal? (car args) "<param-convert>")
        converter)
       (else
        (error "bad parameter syntax"))))))


(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
                   ((param value p old new) ...)
                   ()
                   body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new ((p "<param-convert>") value)) ...)
         (dynamic-wind
          (lambda () (p "<param-set!>" new) ...)
          (lambda () . body)
          (lambda () (p "<param-set!>" old) ...)))))
    ((parameterize ("step")
                   args
                   ((param value) . rest)
                   body)
     (parameterize ("step")
                   ((param value p old new) . args)
                   rest
                   body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step")
                   ()
                   ((param value) ...)
                   body))))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call/cc
               (lambda (handler-k)
                 (guard-k
                  (lambda ()
                    (let ((var condition))
                      (guard-aux
                        (handler-k
                          (lambda ()
                            (raise-continuable condition)))
                        clause ...))))))))
          (lambda ()
            (call-with-values
             (lambda () e1 e2 ...)
             (lambda args
               (guard-k
                 (lambda ()
                   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

(define (values . things)
  (call-with-current-continuation
    (lambda (cont) (apply cont things))))

(define (call-with-port port proc)
  (let ((ret (proc port)))
    (close-port port)
    ret))

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       (let ((len (length args)))
         (letrec-syntax
             ((cl (syntax-rules ::: ()
                    ((cl)
                     (error "no matching clause"))
                    ((cl ((p :::) . body) . rest)
                     (if (= len (length '(p :::)))
                         (apply (lambda (p :::)
                                  . body)
                                args)
                         (cl . rest)))
                    ((cl ((p ::: . tail) . body)
                         . rest)
                     (if (>= len (length '(p :::)))
                         (apply
                          (lambda (p ::: . tail)
                            . body)
                          args)
                         (cl . rest))))))
           (cl (params body0 ...) ...)))))))

(define member
  (case-lambda
    ((obj lst) (member obj lst equal?))
    ((obj lst compare) (if (null? lst)
                           #f
                           (if (compare obj (car lst))
                               lst
                               (member obj (cdr lst) compare))))))

(define (memq obj lst) (member obj lst eq?))
(define (memv obj lst) (member obj lst eqv?))

(define assoc
  (case-lambda
    ((obj lst) (assoc obj lst equal?))
    ((obj lst compare) (if (null? lst)
                           #f
                           (if (compare obj (caar lst))
                               (car lst)
                               (assoc obj (cdr lst) compare))))))

(define (assq obj lst) (assoc obj lst eq?))
(define (assv obj lst) (assoc obj lst eqv?))

(define (map proc . lists)
        (letrec ((simple-map-acc (lambda (proc lsts start end)
                                     (if (null? lsts)
                                         start
                                         (begin (set-cdr! end (list (proc (car lsts))))
                                                (simple-map-acc proc (cdr lsts) start (cdr end))))))
                 (simple-map (lambda (proc list)
                                     (if (null? list)
                                         '()
                                         (let ((p (cons (proc (car list)) '())))
                                              (simple-map-acc proc (cdr list) p p)))))
                 (bad-map (lambda (lsts start end)
                                  (if (memq '() lsts)
                                      start
                                      (begin (set-cdr! end (list (apply proc (simple-map car lsts))))
                                                 (bad-map (simple-map cdr lsts) start (cdr end)))))))
                (if (memq '() lists)
                    '()
                    (let ((p (cons (apply proc (simple-map car lists)) '())))
                         (bad-map (simple-map cdr lists) p p)))))

(define (vector-map proc . lists)
        (letrec ((vector-map-range
             (lambda (lists start end acc)
                     (if (< start end)
                         (begin (vector-set! acc start (apply proc (map (lambda (list) (vector-ref list start)) lists)))
                                (vector-map-range lists (+ start 1) end acc))
                        acc)))
             (len (apply min (map vector-length lists))))
        (vector-map-range lists 0 len (make-vector len))))

(define (string-map proc . lists)
        (letrec ((string-map-range
             (lambda (lists start end acc)
                     (if (< start end)
                         (begin (string-set! acc start (apply proc (map (lambda (list) (string-ref list start)) lists)))
                                (string-map-range lists (+ start 1) end acc))
                        acc)))
             (len (apply min (map string-length lists))))
        (string-map-range lists 0 len (make-string len))))

(define (for-each proc . lists)
        (if (memq '() lists)
            #t
            (begin (apply proc (map car lists))
                   (apply for-each proc (map cdr lists)))))

(define (vector-for-each proc . lists)
        (letrec ((vector-for-each-range
                  (lambda (lists start end)
                     (if (< start end)
                        (begin (apply proc (map (lambda (list) (vector-ref list start)) lists))
                                (vector-for-each-range lists (+ start 1) end))
                         #t))))
        (vector-for-each-range lists 0 (apply min (map vector-length lists)))))

(define (string-for-each proc . lists)
        (letrec ((string-for-each-range
                  (lambda (lists start end)
                     (if (< start end)
                        (begin (apply proc (map (lambda (list) (string-ref list start)) lists))
                                (string-for-each-range lists (+ start 1) end))
                         #t))))
        (string-for-each-range lists 0 (apply min (map string-length lists)))))
(define call/cc call-with-current-continuation)

(import (only (jschememin) library-exists?))

(define-syntax cond-expand
  ;; Extend this to mention all feature ids and libraries
  (syntax-rules (and or not else library)
    ((cond-expand)
     (syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
        more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...)
                  more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand ((library (name ...))
                   body ...)
                  more-clauses ...)
       (if (begin (library-exists? '(name ...)))
           (begin body ...)
           (cond-expand more-clauses ...)))
    ((cond-expand (feature-id body ...)
                  more-clauses ...)
       (if (memq 'feature-id (features))
           (begin body ...)
           (cond-expand more-clauses ...)))))