		assertExpressionValue("(parameterize");
		assertExpressionValue("    ((current-output-port");
		assertExpressionValue("      (open-output-string)))");
		assertExpressionValue("    (display "piece")");
		assertExpressionValue("    (display " by piece ")");
		assertExpressionValue("    (display "by piece.")");
		assertExpressionValue("    (newline)");
		assertExpressionValue("    (get-output-string (current-output-port)))");
		assertExpressionValue("\lev "piece by piece by piece.\backwhack{}n"%");

(define radix
  (make-parameter
   10
   (lambda (x)
     (if (and (exact-integer? x) (<= 2 x 16))
         x
         (error "invalid radix")))))

(define (f n) (number->string n (radix)))

(f 12)                                       \ev "12"
(parameterize ((radix 2))
  (f 12))                                    \ev "1100"
(f 12)                                       \ev "12"

(radix 16)                                   \ev \unspecified

(parameterize ((radix 0))
  (f 12))                                    \ev \scherror%
(load)
(exit)
(guard)
(make-parameter init)
(make-parameter init converter )
(parameterize ((hparam 1 i hvalue 1 i) . . . )
(quasi-quote)
define-values