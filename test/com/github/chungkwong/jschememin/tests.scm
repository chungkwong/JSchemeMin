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
(make-parameter init)
(make-parameter init converter )
(parameterize ((hparam 1 i hvalue 1 i) . . . )
(quasi-quote)
define-values

`(list ,(+ 1 2) 4)  \ev  (list 3 4)
(let ((name 'a)) `(list ,name ',name)) %
          \lev  (list a (quote a))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) %
          \lev  (a 3 4 5 6 b)
`(({\cf foo} ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) %
          \lev  ((foo 7) . cons)
`\#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) %
          \lev  \#(10 5 2 4 3 8)
(let ((foo '(foo bar)) (@baz 'baz))
  `(list ,@foo , @baz))%
          \lev  (list foo bar baz)%
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) %
          \lev  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e)) %
          \lev  (a `(b ,x ,'y d) e)%
(quasiquote (list (unquote (+ 1 2)) 4)) %
          \lev  (list 3 4)
'(quasiquote (list (unquote (+ 1 2)) 4)) %
          \lev  `(list ,(+ 1 2) 4)
     {\em{}i.e.,} (quasiquote (list (unquote (+ 1 2)) 4))%
