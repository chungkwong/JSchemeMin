/*
 * Copyright (C) 2016 Chan Chung Kwong <1m02math@126.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.github.chungkwong.jschememin;
import static com.github.chungkwong.jschememin.SchemeAssert.assertExpressionValue;
import static com.github.chungkwong.jschememin.SchemeAssert.assertStandardOutput;
import static com.github.chungkwong.jschememin.SchemeAssert.expectException;
import org.junit.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class StandardProcedureTest{
	public StandardProcedureTest(){
	}
	@Test
	public void testEquivalent(){
		assertExpressionValue("(eqv? 'a 'a)","#t");
		assertExpressionValue("(eqv? 'a 'b)","#f");
		assertExpressionValue("(eqv? 2 2)","#t");
		assertExpressionValue("(eqv? 2 2.0)","#f");
		assertExpressionValue("(eqv? '() '())","#t");
		assertExpressionValue("(eqv? 100000000 100000000)","#t");
		assertExpressionValue("(eqv? 0.0 +nan.0)","#f");
		assertExpressionValue("(eqv? (cons 1 2) (cons 1 2))","#f");
		assertExpressionValue("(eqv? (lambda () 1) (lambda () 2))","#f");
		assertExpressionValue("(let ((p (lambda (x) x))) (eqv? p p))","#t");
		assertExpressionValue("(eqv? #f 'nil)","#f");
		assertExpressionValue("(let* ((gen-counter (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n)))) "
				+ "(g (gen-counter))) (eqv? g g))","#t");
		assertExpressionValue("(let* ((gen-counter (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n))))) "
				+ "(eqv? (gen-counter) (gen-counter)))","#f");
		assertExpressionValue("(letrec ((f (lambda () (if (eqv? f g) 'f 'both))) (g (lambda () (if (eqv? f g) 'g 'both))))"
				+ " (eqv? f g))","#f");
		assertExpressionValue("(let ((x '(a))) (eqv? x x))","#t");
		assertExpressionValue("(eq? 'a 'a)","#t");
		assertExpressionValue("(eq? (list 'a) (list 'a))","#f");
		assertExpressionValue("(eq? '() '())","#t");
		assertExpressionValue("(eq? car car)","#t");
		assertExpressionValue("(let ((x '(a))) (eq? x x))","#t");
		assertExpressionValue("(let ((x '#())) (eq? x x))","#t");
		assertExpressionValue("(let ((p (lambda (x) x))) (eq? p p))","#t");
		assertExpressionValue("(equal? 'a 'a)","#t");
		assertExpressionValue("(equal? '(a) '(a))","#t");
		assertExpressionValue("(equal? '(a (b) c) '(a (b) c))","#t");
		assertExpressionValue("(equal? \"abc\" \"abc\")","#t");
		assertExpressionValue("(equal? 2 2)","#t");
		assertExpressionValue("(equal? (make-vector 5 'a) (make-vector 5 'a))","#t");
		assertExpressionValue("(equal? '#1=(a b . #1#) '#2=(a b a b . #2#))","#t");
	}
	@Test
	public void testNumber(){
		assertExpressionValue("(number? 3+4i)","#t");
		assertExpressionValue("(number? #t)","#f");
		assertExpressionValue("(number? #\\a)","#f");
		assertExpressionValue("(number? \"hello\")","#f");
		assertExpressionValue("(complex? #t)","#f");
		assertExpressionValue("(complex? 3+4i)","#t");
		assertExpressionValue("(complex? 3)","#t");
		assertExpressionValue("(real? #\\a)","#f");
		assertExpressionValue("(real? 3)","#t");
		assertExpressionValue("(real? -2.5+0i)","#t");
		//assertExpressionValue("(real? -2.5+0.0i)","#f");//R7RS is contradicted with itself
		assertExpressionValue("(real? #e1e10)","#t");
		assertExpressionValue("(real? +inf.0)","#t");
		assertExpressionValue("(real? +nan.0)","#t");
		assertExpressionValue("(rational? \"hello\")","#f");
		assertExpressionValue("(rational? -inf.0)","#f");
		assertExpressionValue("(rational? 3.5)","#t");
		assertExpressionValue("(rational? 6/10)","#t");
		assertExpressionValue("(rational? 6/3)","#t");
		assertExpressionValue("(integer? '())","#f");
		assertExpressionValue("(integer? 3+0i)","#t");
		assertExpressionValue("(integer? 3.0)","#t");
		assertExpressionValue("(integer? 8/4)","#t");
		assertExpressionValue("(exact? 3.0)","#f");
		assertExpressionValue("(exact? #e3.0)","#t");
		assertExpressionValue("(inexact? 3.)","#t");
		assertExpressionValue("(exact-integer? 32)","#t");
		assertExpressionValue("(exact-integer? 32.0)","#f");
		assertExpressionValue("(exact-integer? 32/5)","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (finite? 3))","#t");
		assertExpressionValue("(let () (import (scheme inexact)) (finite? +inf.0))","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (finite? 3.0+inf.0i))","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (infinite? 3))","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (infinite? +inf.0))","#t");
		assertExpressionValue("(let () (import (scheme inexact)) (infinite? +nan.0))","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (infinite? 3.0+inf.0i))","#t");
		assertExpressionValue("(let () (import (scheme inexact)) (nan? -inf.0))","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (nan? +nan.0))","#t");
		assertExpressionValue("(let () (import (scheme inexact)) (nan? 32))","#f");
		assertExpressionValue("(let () (import (scheme inexact)) (nan? +nan.0+5.0i))","#t");
		assertExpressionValue("(let () (import (scheme inexact)) (nan? 1+2i))","#f");
		assertExpressionValue("(= 0/1 0.0 0)","#t");
		assertExpressionValue("(= 0 0+0.1i)","#f");
		assertExpressionValue("(= 3+4i 3.0+8/2i)","#t");
		assertExpressionValue("(= 3+4i +nan.0)","#f");
		assertExpressionValue("(= +inf.0 +nan.0)","#f");
		assertExpressionValue("(< 0 0.0)","#f");
		assertExpressionValue("(< 1/6 1/3)","#t");
		assertExpressionValue("(< -inf.0 3 4 +inf.0)","#t");
		assertExpressionValue("(< -inf.0 3 4 +nan.0 +inf.0)","#f");
		assertExpressionValue("(< -inf.0 3 4 +inf.0 0)","#f");
		assertExpressionValue("(> 0 0.0)","#f");
		assertExpressionValue("(> 1/3 1/6)","#t");
		assertExpressionValue("(> +inf.0 4 -inf.0)","#t");
		assertExpressionValue("(> +inf.0 4 -nan.0 -inf.0)","#f");
		assertExpressionValue("(> +inf.0 3 4 -inf.0 0)","#f");
		assertExpressionValue("(<= 0 0.0)","#t");
		assertExpressionValue("(<= 1/6 1/3)","#t");
		assertExpressionValue("(<= -inf.0 3 4 +inf.0)","#t");
		assertExpressionValue("(<= -inf.0 3 +nan.0 4 +inf.0)","#f");
		assertExpressionValue("(<= -inf.0 3 4 +inf.0 0)","#f");
		assertExpressionValue("(>= 0 0.0)","#t");
		assertExpressionValue("(>= 1/6 1/3)","#f");
		assertExpressionValue("(>= +inf.0 3 -inf.0)","#t");
		assertExpressionValue("(>= +inf.0 3 +nan.0 -inf.0)","#f");
		assertExpressionValue("(>= +inf.0 3 4 +inf.0 0)","#f");
		assertExpressionValue("(zero? 0.0)","#t");
		assertExpressionValue("(zero? 0.0)","#t");
		assertExpressionValue("(zero? 0+0.0i)","#t");
		assertExpressionValue("(zero? 2i)","#f");
		assertExpressionValue("(positive? 2.5)","#t");
		assertExpressionValue("(positive? 0)","#f");
		assertExpressionValue("(positive? -5/2+0i)","#f");
		assertExpressionValue("(negative? 2.5)","#f");
		assertExpressionValue("(negative? 0)","#f");
		assertExpressionValue("(negative? -5/2+0i)","#t");
		assertExpressionValue("(odd? 5)","#t");
		assertExpressionValue("(odd? -2)","#f");
		assertExpressionValue("(even? 5)","#f");
		assertExpressionValue("(even? -2)","#t");
		assertExpressionValue("(max 3 4)","4");
		assertExpressionValue("(max 3.9 4)","4.0");
		assertExpressionValue("(min 3.9)","3.9");
		assertExpressionValue("(min 2 4 -1 5)","-1");

		assertExpressionValue("(let () (import (scheme complex)) (make-rectangular -3 4))","-3+4i");
		assertExpressionValue("(let () (import (scheme complex)) (make-polar 3 2))","3@2");
		assertExpressionValue("(let () (import (scheme complex)) (real-part -3+4i))","-3");
		assertExpressionValue("(let () (import (scheme complex)) (imag-part -3+4i))","4");
		assertExpressionValue("(let () (import (scheme complex)) (magnitude 3@2))","3");
		assertExpressionValue("(let () (import (scheme complex)) (angle 3@2))","2");

		assertExpressionValue("(+ 3 4-2i)","7-2i");
		assertExpressionValue("(+ 3)","3");
		assertExpressionValue("(+)","0");
		assertExpressionValue("(* 4)","4");
		assertExpressionValue("(*)","1");
		assertExpressionValue("(- 3 4)","-1");
		assertExpressionValue("(- 3 4 5)","-6");
		assertExpressionValue("(- 3)","-3");
		assertExpressionValue("(/ 3 4 5)","3/20");
		assertExpressionValue("(/ 3)","1/3");
		assertExpressionValue("(abs -7)","7");
		assertExpressionValue("(abs 8/5)","8/5");
		assertExpressionValue("(let-values (((q r) (truncate/ 5 2))) (list q r))","'(2 1)");
		assertExpressionValue("(let-values (((q r) (truncate/ -5 2))) (list q r))","'(-2 -1)");
		assertExpressionValue("(let-values (((q r) (truncate/ 5 -2))) (list q r))","'(-2 1)");
		assertExpressionValue("(let-values (((q r) (truncate/ -5 -2))) (list q r))","'(2 -1)");
		assertExpressionValue("(let-values (((q r) (truncate/ -5.0 -2))) (list q r))","'(2.0 -1.0)");
		assertExpressionValue("(let-values (((q r) (floor/ 5 2))) (list q r))","'(2 1)");
		assertExpressionValue("(let-values (((q r) (floor/ -5 2))) (list q r))","'(-3 1)");
		assertExpressionValue("(let-values (((q r) (floor/ 5 -2))) (list q r))","'(-3 -1)");
		assertExpressionValue("(let-values (((q r) (floor/ -5 -2))) (list q r))","'(2 -1)");
		assertExpressionValue("(truncate-quotient 5 2)","2");
		assertExpressionValue("(truncate-remainder 5 2)","1");
		assertExpressionValue("(quotient 5 2)","2");
		assertExpressionValue("(remainder 5 2)","1");
		assertExpressionValue("(floor-quotient 5 2)","2");
		assertExpressionValue("(floor-remainder 5 2)","1");
		assertExpressionValue("(modulo 5 2)","1");
		assertExpressionValue("(truncate-quotient -5 2)","-2");
		assertExpressionValue("(truncate-remainder -5 2)","-1");
		assertExpressionValue("(quotient -5 2)","-2");
		assertExpressionValue("(remainder -5 2)","-1");
		assertExpressionValue("(floor-quotient -5 2)","-3");
		assertExpressionValue("(floor-remainder -5 2)","1");
		assertExpressionValue("(modulo -5 2)","1");
		assertExpressionValue("(gcd 32 -36)","4");
		assertExpressionValue("(gcd 4)","4");
		assertExpressionValue("(gcd)","0");
		assertExpressionValue("(lcm 32 -36)","288");
		assertExpressionValue("(lcm 32.0 -36)","288.0");
		assertExpressionValue("(lcm 4)","4");
		assertExpressionValue("(lcm)","1");
		assertExpressionValue("(numerator (/ 6 4))","3");
		assertExpressionValue("(denominator (/ 6 4))","2");
		assertExpressionValue("(denominator (inexact (/ 6 4)))","2.0");
		assertExpressionValue("(floor -4.3)","-5.0");
		assertExpressionValue("(ceiling -4.3)","-4.0");
		assertExpressionValue("(truncate -4.3)","-4.0");
		assertExpressionValue("(round -4.3)","-4.0");
		assertExpressionValue("(floor 3.5)","3.0");
		assertExpressionValue("(ceiling 3.5)","4.0");
		assertExpressionValue("(truncate 3.5)","3.0");
		assertExpressionValue("(round 3.5)","4.0");
		assertExpressionValue("(round 7/2)","4");
		assertExpressionValue("(round 7)","7");
		assertExpressionValue("(rationalize (exact .3) 1/10)","1/3");
		assertExpressionValue("(rationalize .3 1/10)","#i1/3");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (exp 0) 1)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (log 1) 0)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (log (exp -1.4)) -1.4)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (log 8 4) 1.5)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (sin 0) 0)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (cos 0) 1)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (tan 0) 0)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (asin 0) 0)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (acos 1) 0)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (atan 0) 0)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (tan (atan 2 1)) 2)) 1e-12))","#t");
		assertExpressionValue("(square 42)","1764");
		assertExpressionValue("(square 2.0)","4.0");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (sqrt 9) 3)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (sqrt -1) +i)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (expt 4 2.5) 32)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (expt 0 0) 1)) 1e-12))","#t");
		assertExpressionValue("(let () (import (scheme inexact) (scheme complex)) (< (magnitude (- (expt 0 2+3i) 0)) 1e-12))","#t");
		assertExpressionValue("(let-values (((q r) (exact-integer-sqrt -1))) (list q r))","'(0 -1)");
		assertExpressionValue("(let-values (((q r) (exact-integer-sqrt 4))) (list q r))","'(2 0)");
		assertExpressionValue("(let-values (((q r) (exact-integer-sqrt 5))) (list q r))","'(2 1)");
		assertExpressionValue("(string->number \"100\")","100");
		assertExpressionValue("(string->number \"100\" 16)","256");
		assertExpressionValue("(string->number \"1e2\")","100.0");
		assertExpressionValue("(string->number \"1011\")","1011");
		assertExpressionValue("(string->number \"1011\" 2)","#b1011");
		assertExpressionValue("(string->number (number->string 27))","27");
		assertExpressionValue("(string->number (number->string 27 8) 8)","27");
		assertExpressionValue("(string->number (number->string 27 16) 16)","27");
	}
	@Test
	public void testBoolean(){
		assertExpressionValue("#t","#t");
		assertExpressionValue("#f","#f");
		assertExpressionValue("'#f","#f");
		assertExpressionValue("(not #t)","#f");
		assertExpressionValue("(not 3)","#f");
		assertExpressionValue("(not (list 3))","#f");
		assertExpressionValue("(not #f)","#t");
		assertExpressionValue("(not '())","#f");
		assertExpressionValue("(not (list))","#f");
		assertExpressionValue("(not 'nil)","#f");
		assertExpressionValue("(boolean? #f)","#t");
		assertExpressionValue("(boolean? 0)","#f");
		assertExpressionValue("(boolean? '())","#f");
		assertExpressionValue("(boolean=? #t #t)","#t");
		assertExpressionValue("(boolean=? #t #f)","#f");
		assertExpressionValue("(boolean=? #f #f #f)","#t");
		assertExpressionValue("(boolean=? #f #f #t)","#f");
	}
	@Test
	public void testList(){
		assertExpressionValue("(pair? '(a . b))","#t");
		assertExpressionValue("(pair? '(a b c))","#t");
		assertExpressionValue("(pair? '())","#f");
		assertExpressionValue("(pair? '#(a b))","#f");
		assertExpressionValue("(cons 'a '())","'(a)");
		assertExpressionValue("(cons '(a) '(b c d))","'((a) b c d)");
		assertExpressionValue("(cons \"a\" '(b c))","'(\"a\" b c)");
		assertExpressionValue("(cons 'a 3)","'(a . 3)");
		assertExpressionValue("(cons '(a b) 'c)","'((a b) . c)%");
		assertExpressionValue("(car '(a b c))","'a");
		assertExpressionValue("(car '((a) b c d))","'(a)");
		assertExpressionValue("(car '(1 . 2))","1");
		expectException("(car '())");
		assertExpressionValue("(cdr '((a) b c d))","'(b c d)");
		assertExpressionValue("(cdr '(1 . 2))","2");
		expectException("(cdr '())");
		assertExpressionValue("(let ((x (cons 1 2))) (set-car! x 3) x)","(cons 3 2)");
		assertExpressionValue("(let ((x (cons 1 2))) (set-cdr! x 3) x)","(cons 1 3)");
		assertExpressionValue("(caar (cons (cons 1 2) (cons 3 4)))","1");
		assertExpressionValue("(cdar (cons (cons 1 2) (cons 3 4)))","2");
		assertExpressionValue("(cadr (cons (cons 1 2) (cons 3 4)))","3");
		assertExpressionValue("(cddr (cons (cons 1 2) (cons 3 4)))","4");
		assertExpressionValue("(let () (import (scheme cxr)) (caaar '(((a)))))","'a");
		assertExpressionValue("(let () (import (scheme cxr)) (cddddr '(1 2 3 4 5)))","'(5)");
		assertExpressionValue("(null? #t)","#f");
		assertExpressionValue("(null? #\\a)","#f");
		assertExpressionValue("(null? \"\")","#f");
		assertExpressionValue("(null? 2)","#f");
		assertExpressionValue("(null? '())","#t");
		assertExpressionValue("(null? '(1 2))","#f");
		assertExpressionValue("(list? '())","#t");
		assertExpressionValue("(list? '(1))","#t");
		assertExpressionValue("(list? '(1 2))","#t");
		assertExpressionValue("(list? '(1 2 . 3))","#f");
		assertExpressionValue("(list? '#88=(1 2 . #88#))","#f");
		assertExpressionValue("(list? 1)","#f");
		assertExpressionValue("(list? #(1 2))","#f");
		assertExpressionValue("(list? #t)","#f");
		assertExpressionValue("(list? \"\")","#f");
		assertExpressionValue("(make-list 2 3)","'(3 3)");
		assertExpressionValue("(length (make-list 5))","5");
		assertExpressionValue("(list 'a (+ 3 4) 'c)","'(a 7 c)");
		assertExpressionValue("(list)","'()");
		assertExpressionValue("(length '(a b c))","3");
		assertExpressionValue("(length '(a (b) (c d e)))","3");
		assertExpressionValue("(length '())","0");
		assertExpressionValue("(append '(x) '(y))","'(x y)");
		assertExpressionValue("(append '(a) '(b c d))","'(a b c d)");
		assertExpressionValue("(append '(a (b)) '((c)))","'(a (b) (c))");
		assertExpressionValue("(append '(a b) '(c . d))","'(a b c . d)");
		assertExpressionValue("(append '() 'a)","'a");
		assertExpressionValue("(reverse '(a b c))","'(c b a)");
		assertExpressionValue("(reverse '(a (b c) d (e (f))))","'((e (f)) d (b c) a)");
		assertExpressionValue("(list-tail '(a (b c) d (e (f))) 2)","'(d (e (f)))");
		assertExpressionValue("(list-ref '(a b c d) 2)","'c");
		assertExpressionValue("(list-ref '(a b c d) (exact (round 1.8)))","'c");
		assertExpressionValue("(let ((ls (list 'one 'two 'five!))) (list-set! ls 2 'three) ls)","'(one two three)");
		assertExpressionValue("(memq 'a '(a b c))","'(a b c)");
		assertExpressionValue("(memq 'b '(a b c))","'(b c)");
		assertExpressionValue("(memq 'a '(b c d))","#f");
		assertExpressionValue("(memq (list 'a) '(b (a) c))","#f");
		assertExpressionValue("(member (list 'a) '(b (a) c))","'((a) c)");
		assertExpressionValue("(begin (import (scheme char)) (member \"B\" '(\"a\" \"b\" \"c\") string-ci=?))","'(\"b\" \"c\")");
		assertExpressionValue("(memv 101 '(100 101 102))","'(101 102)");
		assertExpressionValue("(assq 'a '((a 1) (b 2) (c 3)))","'(a 1)");
		assertExpressionValue("(assq 'b '((a 1) (b 2) (c 3)))","'(b 2)");
		assertExpressionValue("(assq 'd '((a 1) (b 2) (c 3)))","#f");
		assertExpressionValue("(assq (list 'a) '(((a)) ((b)) ((c))))","#f");
		assertExpressionValue("(assoc (list 'a) '(((a)) ((b)) ((c))))","'((a))");
		assertExpressionValue("(assoc 2.0 '((1 1) (2 4) (3 9)) =)","'(2 4)");
		assertExpressionValue("(assv 5 '((2 3) (5 7) (11 13)))","'(5 7)");
		assertExpressionValue("(let ((a '(1 8 2 8))) (let ((b (list-copy a))) (set-car! b 3) b))","'(3 8 2 8)");
		assertExpressionValue("(let ((a '(1 8 2 8))) (let ((b (list-copy a))) (set-car! b 3) a))","'(1 8 2 8)");
	}
	@Test
	public void testSymbol(){
		assertExpressionValue("(symbol? 'foo)","#t");
		assertExpressionValue("(symbol? (car '(a b)))","#t");
		assertExpressionValue("(symbol? \"bar\")","#f");
		assertExpressionValue("(symbol? 'nil)","#t");
		assertExpressionValue("(symbol? '())","#f");
		assertExpressionValue("(symbol? #f)","#f");
		assertExpressionValue("(symbol=? 'hello 'hello)","#t");
		assertExpressionValue("(symbol=? 'hello 'hell)","#f");
		assertExpressionValue("(symbol=? 'hello 'hello 'hello)","#t");
		assertExpressionValue("(symbol=? 'hello 'hello 'hell)","#f");
		assertExpressionValue("(symbol->string 'flying-fish)","\"flying-fish\"");
		assertExpressionValue("(symbol->string 'Martin)","\"Martin\"");
		assertExpressionValue("(symbol->string (string->symbol \"Malvina\"))","\"Malvina\"");
		assertExpressionValue("(string->symbol \"mISSISSIppi\")","'mISSISSIppi");
		assertExpressionValue("(eqv? 'bitBlt (string->symbol \"bitBlt\"))","#t");
		assertExpressionValue("(eqv? 'LollyPop (string->symbol (symbol->string 'LollyPop)))","#t");
		assertExpressionValue("(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))","#t");
	}
	@Test
	public void testCharacter(){
		assertExpressionValue("(char? #\\space)","#t");
		assertExpressionValue("(char? \"a\")","#f");
		assertExpressionValue("(char? '())","#f");
		assertExpressionValue("(char? 'a)","#f");
		assertExpressionValue("(char? '(#\\a))","#f");
		assertExpressionValue("(char=? #\\a  #\\A)","#f");
		assertExpressionValue("(char=? #\\  #\\space)","#t");
		assertExpressionValue("(char=? #\\a #\\a #\\a)","#t");
		assertExpressionValue("(char=? #\\a #\\b)","#f");
		assertExpressionValue("(char=? #\\a #\\b #\\b)","#f");
		assertExpressionValue("(char<? #\\a  #\\Z)","#f");
		assertExpressionValue("(char<? #\\  #\\space)","#f");
		assertExpressionValue("(char<? #\\a  #\\b)","#t");
		assertExpressionValue("(char<? #\\a #\\b #\\c)","#t");
		assertExpressionValue("(char<? #\\a #\\b #\\a)","#f");
		assertExpressionValue("(char<? #\\b #\\a)","#f");
		assertExpressionValue("(char>? #\\a  #\\Z)","#t");
		assertExpressionValue("(char>? #\\  #\\space)","#f");
		assertExpressionValue("(char>? #\\a  #\\b)","#f");
		assertExpressionValue("(char>? #\\a #\\b #\\c)","#f");
		assertExpressionValue("(char>? #\\c #\\b #\\a)","#t");
		assertExpressionValue("(char>? #\\b #\\a)","#t");
		assertExpressionValue("(char<=? #\\  #\\space)","#t");
		assertExpressionValue("(char<=? #\\a  #\\b)","#t");
		assertExpressionValue("(char<=? #\\a #\\b #\\c)","#t");
		assertExpressionValue("(char<=? #\\a #\\b #\\a)","#f");
		assertExpressionValue("(char<=? #\\b #\\a)","#f");
		assertExpressionValue("(char>=? #\\  #\\space)","#t");
		assertExpressionValue("(char>=? #\\a  #\\b)","#f");
		assertExpressionValue("(char>=? #\\a #\\b #\\c)","#f");
		assertExpressionValue("(char>=? #\\c #\\b #\\a)","#t");
		assertExpressionValue("(char>=? #\\b #\\a)","#t");
		assertExpressionValue("(integer->char (char->integer #\\a))","#\\a");
		assertExpressionValue("(char->integer (integer->char 50))","50");
		assertExpressionValue("(let () (import (scheme char)) (char-alphabetic? #\\a))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-alphabetic? #\\Z))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-alphabetic? #\\5))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-alphabetic? #\\ ))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-upper-case? #\\a))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-upper-case? #\\Z))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-upper-case? #\\5))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-upper-case? #\\ ))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-lower-case? #\\a))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-lower-case? #\\Z))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-lower-case? #\\5))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-lower-case? #\\ ))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-whitespace? #\\a))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-whitespace? #\\Z))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-whitespace? #\\5))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-whitespace? #\\ ))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-numeric? #\\a))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-numeric? #\\5))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-numeric? #\\ ))","#f");
		assertExpressionValue("(let () (import (scheme char)) (digit-value #\\3))","3");
		assertExpressionValue("(let () (import (scheme char)) (digit-value #\\x0664))","4");
		assertExpressionValue("(let () (import (scheme char)) (digit-value #\\x0AE6))","0");
		assertExpressionValue("(let () (import (scheme char)) (digit-value #\\x0EA6))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-upcase #\\a))","#\\A");
		assertExpressionValue("(let () (import (scheme char)) (char-upcase #\\Z))","#\\Z");
		assertExpressionValue("(let () (import (scheme char)) (char-downcase #\\a))","#\\a");
		assertExpressionValue("(let () (import (scheme char)) (char-downcase #\\Z))","#\\z");
		assertExpressionValue("(let () (import (scheme char)) (char-foldcase #\\a))","#\\a");
		assertExpressionValue("(let () (import (scheme char)) (char-foldcase #\\Z))","#\\z");
		assertExpressionValue("(let () (import (scheme char)) (char-ci=? #\\a #\\A))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-ci<? #\\a #\\A))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-ci>? #\\a #\\A))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-ci<=? #\\a #\\A))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-ci>=? #\\a #\\A))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-ci=? #\\a #\\Z))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-ci<? #\\a #\\Z))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-ci>? #\\a #\\Z))","#f");
		assertExpressionValue("(let () (import (scheme char)) (char-ci<=? #\\a #\\Z))","#t");
		assertExpressionValue("(let () (import (scheme char)) (char-ci>=? #\\a #\\Z))","#f");
	}
	@Test
	public void testString(){
		assertExpressionValue("(string? (car '(#\\a b)))","#f");
		assertExpressionValue("(string? \"bar\")","#t");
		assertExpressionValue("(string? 'nil)","#f");
		assertExpressionValue("(string? '())","#f");
		assertExpressionValue("(string? #t)","#f");
		assertExpressionValue("(make-string 0 #\\a)","\"\"");
		assertExpressionValue("(make-string 3 #\\a)","\"aaa\"");
		assertExpressionValue("(string-length (make-string 0))","0");
		assertExpressionValue("(string-length (make-string 4))","4");
		assertExpressionValue("(string)","\"\"");
		assertExpressionValue("(string #\\a #\\b)","\"ab\"");
		assertExpressionValue("(string-length \"\")","0");
		assertExpressionValue("(string-length \"hello\")","5");
		assertExpressionValue("(string-ref \"world\" 0)","#\\w");
		assertExpressionValue("(string-ref \"world\" 2)","#\\r");
		assertExpressionValue("(string-ref \"world\" 4)","#\\d");
		assertExpressionValue("(let ((str (make-string 4))) (string-set! str 2 #\\a) (string-ref str 2))","#\\a");
		assertExpressionValue("(let ((str (make-string 4))) (string-set! str 2 #\\a) (string-length str))","4");
		assertExpressionValue("(string=? \"\" \"\")","#t");
		assertExpressionValue("(string=? \"hello\" \"hello\")","#t");
		assertExpressionValue("(string=? \"hell\" \"hello\")","#f");
		assertExpressionValue("(string=? \"hello\" \"heLlo\")","#f");
		assertExpressionValue("(string<? \"\" \"\")","#f");
		assertExpressionValue("(string<? \"hello\" \"hello\")","#f");
		assertExpressionValue("(string<? \"hell\" \"hello\")","#t");
		assertExpressionValue("(string<? \"hell\" \"heZlo\")","#f");
		assertExpressionValue("(string>? \"\" \"\")","#f");
		assertExpressionValue("(string>? \"hello\" \"hello\")","#f");
		assertExpressionValue("(string>? \"hell\" \"hello\")","#f");
		assertExpressionValue("(string>? \"hell\" \"heZlo\")","#t");
		assertExpressionValue("(string<=? \"\" \"\")","#t");
		assertExpressionValue("(string<=? \"hello\" \"hello\")","#t");
		assertExpressionValue("(string<=? \"hell\" \"hello\")","#t");
		assertExpressionValue("(string<=? \"hell\" \"heZlo\")","#f");
		assertExpressionValue("(string>=? \"\" \"\")","#t");
		assertExpressionValue("(string>=? \"hello\" \"hello\")","#t");
		assertExpressionValue("(string>=? \"hell\" \"hello\")","#f");
		assertExpressionValue("(string>=? \"hell\" \"heZlo\")","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci=? \"\" \"\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci=? \"hello\" \"helLo\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci=? \"hell\" \"helLo\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci=? \"heZlo\" \"hell\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<? \"\" \"\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<? \"hello\" \"helLo\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<? \"hell\" \"helLo\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<? \"heZlo\" \"hell\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>? \"\" \"\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>? \"hello\" \"helLo\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>? \"hell\" \"helLo\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>? \"heZlo\" \"hell\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<=? \"\" \"\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<=? \"hello\" \"helLo\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<=? \"hell\" \"helLo\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci<=? \"heZlo\" \"hell\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>=? \"\" \"\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>=? \"hello\" \"helLo\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>=? \"hell\" \"helLo\"))","#f");
		assertExpressionValue("(let () (import (scheme char)) (string-ci>=? \"heZlo\" \"hell\"))","#t");
		assertExpressionValue("(let () (import (scheme char)) (string-upcase \"he Llo5\"))","\"HE LLO5\"");
		assertExpressionValue("(let () (import (scheme char)) (string-downcase \"he Llo5\"))","\"he llo5\"");
		assertExpressionValue("(let () (import (scheme char)) (string-foldcase \"he Llo5\"))","\"he llo5\"");
		assertExpressionValue("(substring \"hello\" 5 5)","\"\"");
		assertExpressionValue("(substring \"hello\" 2 4)","\"ll\"");
		assertExpressionValue("(string-append)","\"\"");
		assertExpressionValue("(string-append \"hello\")","\"hello\"");
		assertExpressionValue("(string-append \"i\" \"hate\" \"you\")","\"ihateyou\"");
		assertExpressionValue("(string->list \"\")","'()");
		assertExpressionValue("(string->list \"hello\")","'(#\\h #\\e #\\l #\\l #\\o)");
		assertExpressionValue("(string->list \"hello\" 3)","'(#\\l #\\o)");
		assertExpressionValue("(string->list \"hello\" 3 4)","'(#\\l)");
		assertExpressionValue("(list->string '())","\"\"");
		assertExpressionValue("(list->string '(#\\h #\\e))","\"he\"");
		assertExpressionValue("(string-copy \"hello\")","\"hello\"");
		assertExpressionValue("(string-copy \"hello\" 2)","\"llo\"");
		assertExpressionValue("(string-copy \"hello\" 2 4)","\"ll\"");
		assertExpressionValue("(let ((str (string #\\a #\\b #\\c))) (string-set! (string-copy str) 2 #\\d) str)","\"abc\"");
		assertExpressionValue("(let ((str \"hello\")) (string-copy! str 2 \"big\") str)","\"hebig\"");
		assertExpressionValue("(let ((str \"hello\")) (string-copy! str 2 \"big\") str)","\"hebig\"");
		assertExpressionValue("(let ((str \"hello\")) (string-copy! str 2 \"big\" 1) str)","\"heigo\"");
		assertExpressionValue("(let ((str \"hello\")) (string-copy! str 2 \"big\" 1 2) str)","\"heilo\"");
		assertExpressionValue("(let ((str \"hello\")) (string-fill! str #\\a) str)","\"aaaaa\"");
		assertExpressionValue("(let ((str \"hello\")) (string-fill! str #\\a 2) str)","\"heaaa\"");
		assertExpressionValue("(let ((str \"hello\")) (string-fill! str #\\a 2 4) str)","\"heaao\"");
	}
	@Test
	public void testVector(){
		assertExpressionValue("(make-vector 2 12)","#(12 12)");
		assertExpressionValue("(vector-length (make-vector 5))","5");
		assertExpressionValue("(vector-length #())","0");
		assertExpressionValue("(vector-length #(1 2 7))","3");
		assertExpressionValue("(vector #t 3 5 #f 3 5)","#(#t 3 5 #f 3 5)");
		assertExpressionValue("(vector)","#()");
		assertExpressionValue("(vector 'a 'b 'c)","#(a b c)");
		assertExpressionValue("(vector-ref '#(1 1 2 3 5 8 13 21) 5)","8");
		assertExpressionValue("(let () (import (scheme inexact)) (vector-ref '#(1 1 2 3 5 8 13 21) (exact (round (* 2 (acos -1))))))",
				"13");
		assertExpressionValue("(let ((v (vector 1 2 3 4))) (vector-set! v 1 3) v)","#(1 3 3 4)");
		assertExpressionValue("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) vec)",
				"#(0 (\"Sue\" \"Sue\") \"Anna\")");
		assertExpressionValue("(vector-copy #(1 2 3 4 5) 2 4))","#(3 4)");
		assertExpressionValue("(vector-copy #(1 2 3 4 5) 2))","#(3 4 5)");
		assertExpressionValue("(vector-copy #(1 2 3 4 5)))","#(1 2 3 4 5)");
		assertExpressionValue("(let ((a (vector 1 2 3 4 5)) (b (vector 10 20 30 40 50))) (vector-copy! b 1 a 0 2) b)"
				,"#(10 1 2 40 50)");
		assertExpressionValue("(let ((a (vector 1 2 3 4)) (b (vector 10 20 30 40 50))) (vector-copy! b 1 a 0) b)"
				,"#(10 1 2 3 4)");
		assertExpressionValue("(let ((a (vector 1 2 3 4)) (b (vector 10 20 30 40 50))) (vector-copy! b 1 a) b)"
				,"#(10 1 2 3 4)");
		assertExpressionValue("(vector->list #(1 a b))","'(1 a b)");
		assertExpressionValue("(vector->list #(1 a b) 1)","'(a b)");
		assertExpressionValue("(vector->list #(1 a b) 1 2)","'(a)");
		assertExpressionValue("(vector->list '#(dah dah didah))","'(dah dah didah)");
		assertExpressionValue("(vector->list '#(dah dah didah) 1 2)","'(dah)");
		assertExpressionValue("(list->vector '(dididit dah))","#(dididit dah)");
		assertExpressionValue("(list->vector '(1 a))","#(1 a)");
		assertExpressionValue("(list->vector '())","#()");
		assertExpressionValue("(vector->string #())","\"\"");
		assertExpressionValue("(vector->string #(#\\a #\\b #\\c))","\"abc\"");
		assertExpressionValue("(vector->string #(#\\a #\\b #\\c) 1)","\"bc\"");
		assertExpressionValue("(vector->string #(#\\a #\\b #\\c) 1 2)","\"b\"");
		assertExpressionValue("(string->vector \"he我llo\")","#(#\\h #\\e #\\我 #\\l #\\l #\\o)");
		assertExpressionValue("(string->vector \"he我llo\" 4)","#(#\\l #\\o)");
		assertExpressionValue("(string->vector \"he我llo\" 4 5)","#(#\\l)");
		assertExpressionValue("(vector-append)","#()");
		assertExpressionValue("(vector-append #(0 1 2) #(3 4 5))","#(0 1 2 3 4 5)");
		assertExpressionValue("(vector-append #(a b c) #(d e f))","#(a b c d e f)");
		assertExpressionValue("(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash) a)","#(smash smash smash smash smash)");
		assertExpressionValue("(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash 2) a)","#(1 2 smash smash smash)");
		assertExpressionValue("(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash 2 4) a)","#(1 2 smash smash 5)");
	}
	@Test
	public void testByteVector(){
		assertExpressionValue("(make-bytevector 2 12)","#u8(12 12)");
		assertExpressionValue("(bytevector-length (make-bytevector 5))","5");
		assertExpressionValue("(bytevector-length #u8())","0");
		assertExpressionValue("(bytevector 1 3 5 1 3 5)","#u8(1 3 5 1 3 5)");
		assertExpressionValue("(bytevector)","#u8()");
		assertExpressionValue("(bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5)","8");
		assertExpressionValue("(let ((bv (bytevector 1 2 3 4))) (bytevector-u8-set! bv 1 3) bv)","#u8(1 3 3 4)");
		assertExpressionValue("(bytevector-copy #u8(1 2 3 4 5) 2 4))","#u8(3 4)");
		assertExpressionValue("(bytevector-copy #u8(1 2 3 4 5) 2))","#u8(3 4 5)");
		assertExpressionValue("(bytevector-copy #u8(1 2 3 4 5)))","#u8(1 2 3 4 5)");
		assertExpressionValue("(let ((a (bytevector 1 2 3 4 5)) (b (bytevector 10 20 30 40 50))) (bytevector-copy! b 1 a 0 2) b)"
				,"#u8(10 1 2 40 50)");
		assertExpressionValue("(let ((a (bytevector 1 2 3 4)) (b (bytevector 10 20 30 40 50))) (bytevector-copy! b 1 a 0) b)"
				,"#u8(10 1 2 3 4)");
		assertExpressionValue("(let ((a (bytevector 1 2 3 4)) (b (bytevector 10 20 30 40 50))) (bytevector-copy! b 1 a) b)"
				,"#u8(10 1 2 3 4)");
		assertExpressionValue("(bytevector-append)","#u8()");
		assertExpressionValue("(bytevector-append #u8(0 1 2) #u8(3 4 5))","#u8(0 1 2 3 4 5)");
		assertExpressionValue("(string->utf8 \"hello 我\")","#u8(104 101 108 108 111 32 230 136 145)");
		assertExpressionValue("(string->utf8 \"hello 我\" 2)","#u8(108 108 111 32 230 136 145)");
		assertExpressionValue("(string->utf8 \"hello 我\" 2 4)","#u8(108 108)");
		assertExpressionValue("(utf8->string #u8(#x41))","\"A\"");
		assertExpressionValue("(utf8->string #u8(#x41) 0)","\"A\"");
		assertExpressionValue("(utf8->string #u8(#x41) 0 0)","\"\"");
	}
	@Test
	public void testControl(){
		assertExpressionValue("(procedure? car)","#t");
		assertExpressionValue("(procedure? (lambda (x) (+ x x)))","#t");
		assertExpressionValue("(procedure? '(lambda (x) (+ x x)))","#f");
		assertExpressionValue("(procedure? if)","#f");
		assertExpressionValue("(procedure? let)","#f");
		assertExpressionValue("(call-with-current-continuation procedure?)","#t");
		assertExpressionValue("(apply + (list 3 4))","7");
		assertExpressionValue("(apply + 1 (list 3 4))","8");
		assertExpressionValue("(let ((compose (lambda (f g) (lambda args (f (apply g args)))))) ((compose square *) 3 4))","144");
		assertExpressionValue("(call-with-current-continuation (lambda (exit) (exit 7) #t))","7");
		assertExpressionValue("(call-with-current-continuation (lambda (exit) "
				+ "(for-each (lambda (x) (if (negative? x) (exit x))) '(54 0 37 -3 245 19)) #t))","-3");
		assertExpressionValue("(let ((list-length (lambda (obj) (call-with-current-continuation (lambda (return) "
				+ "(letrec ((r (lambda (obj) (cond ((null? obj) 0) ((pair? obj) (+ (r (cdr obj)) 1)) (else (return #f)))))) (r obj))))))) "
				+ "(list-length '(1 2 3 4)))","4");
		assertExpressionValue("(let ((list-length (lambda (obj) (call-with-current-continuation (lambda (return) "
				+ "(letrec ((r (lambda (obj) (cond ((null? obj) 0) ((pair? obj) (+ (r (cdr obj)) 1)) (else (return #f)))))) (r obj))))))) "
				+ "(list-length '(a b . c)))","#f");
		assertStandardOutput("(dynamic-wind (lambda () (write-char #\\a)) (lambda () (write-char #\\b)) (lambda () (write-char #\\c) (flush-output-port)))","abc");
		assertExpressionValue("(let ((path '()) (c #f)) (let ((add (lambda (s) (set! path (cons s path))))) "
				+ "(dynamic-wind (lambda () (add 'connect)) "
				+ "(lambda () (add (call-with-current-continuation (lambda (c0) (set! c c0) 'talk1)))) "
				+ "(lambda () (add 'disconnect))) "
				+ "(if (< (length path) 4) (c 'talk2) (reverse path))))",
				"'(connect talk1 disconnect connect talk2 disconnect)");
		assertExpressionValue("(call/cc (lambda (x) (dynamic-wind (lambda () (write-char #\\a)) "
				+ "(lambda () (x 8) (write-char #\\b)) (lambda () (write-char #\\c)))))","8");
		assertStandardOutput("(call/cc (lambda (x) (dynamic-wind (lambda () (write-char #\\a)) "
				+ "(lambda () (x 8) (write-char #\\b)) (lambda () (write-char #\\c)))))","ac");
		assertExpressionValue("(call-with-values (lambda () (values 4 5)) (lambda (a b) b))","5");
		assertExpressionValue("(call-with-values * -)","-1");
		assertExpressionValue("(let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4))  v)"
				,"#(0 1 4 9 16)");
		assertExpressionValue("(let ((v '())) (string-for-each (lambda (c) (set! v (cons (char->integer c) v))) \"abcde\") v)"
				,"'(101 100 99 98 97)");
		assertExpressionValue("(let ((v (make-list 5))) (vector-for-each (lambda (i) (list-set! v i (* i i))) #(0 1 2 3 4)) v)"
				,"'(0 1 4 9 16)");
		assertExpressionValue("(map cadr '((a b) (d e) (g h)))","'(b e h)");
		assertExpressionValue("(map (lambda (n) (+ n n)) '(1 2 3 4 5))","'(2 4 6 8 10)");
		assertExpressionValue("(map + '(1 2 3) '(4 5 6 7))","'(5 7 9)");
		assertExpressionValue("(let ((count 0)) (map (lambda (ignored) (set! count (+ count 1)) count) '(a b)))","'(1 2)");
		assertExpressionValue("(begin (import (scheme char)) (string-map char-foldcase \"AbdEgH\"))","\"abdegh\"");
		assertExpressionValue("(string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) \"HAL\")","\"IBM\"");
		assertExpressionValue("(begin (import (scheme char)) (string-map (lambda (c k) ((if (eqv? k #\\u) char-upcase char-downcase) c))"
				+ "\"studlycaps xxx\" \"ululululul\"))","\"StUdLyCaPs\"");
		assertExpressionValue("(vector-map cadr '#((a b) (d e) (g h)))","#(b e h)");
		assertExpressionValue("(vector-map (lambda (n) (* n n)) #(1 2 3 4 5))","#(1 4 9 16 25)");
		assertExpressionValue("(vector-map + #(1 2 3) #(4 5 6 7))","#(5 7 9)");
		assertExpressionValue("(let ((count 0)) (vector-map (lambda (ignored) (set! count (+ count 1)) count) #(a b)))","#(1 2)");
	}
	@Test
	public void testException(){
		expectException("(raise 'bad)");
		assertExpressionValue("(with-exception-handler (lambda (e) 12) (lambda () (+ (raise-continuable #t) 2)))","14");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display \"something went wrong\\n\") (flush-output-port)) "
				+ "(lambda () (+ 1 (raise 'an-error))))" ,
				"something went wrong\n");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (error-object? x)) (flush-output-port)) "
				+ "(lambda () (+ 1 (raise 'an-error))))" ,
				"#f");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (error-object? x)) (flush-output-port)) "
				+ "(lambda () (+ 1 (error \"bad\" '(1 2 3)))))" ,
				"#t");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (read-error? x)) (flush-output-port)) "
				+ "(lambda () (+ 1 (error \"bad\" '(1 2 3)))))" ,
				"#f");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (file-error? x)) (flush-output-port)) "
				+ "(lambda () (+ 1 (error \"bad\" '(1 2 3)))))" ,
				"#f");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (error-object-message x)) (flush-output-port)) "
				+ "(lambda () (+ 1 (error \"bad\" 1 2 3))))" ,
				"bad");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (error-object-irritants x)) (flush-output-port)) "
				+ "(lambda () (+ 1 (error \"bad\" 1 2 3))))" ,
				"(1 2 3)");
		assertStandardOutput("(with-exception-handler (lambda (x) (import (scheme write)) (display (error-object-message x)) (flush-output-port)) "
				+ "(lambda () (with-exception-handler (lambda (x) (import (scheme write)) (display (error-object-irritants x)) (flush-output-port)) (lambda () (+ 1 (error \"bad\" 1 2 3))))))",
				"(1 2 3)bad");
		assertExpressionValue("(begin (import (scheme write)) (call-with-current-continuation (lambda (k) (with-exception-handler "
				+ "(lambda (x) (display \"condition: \") (write x) (flush-output-port) (k 'exception)) "
				+ "(lambda () (+ 1 (raise 'an-error)))))))","'exception");
		assertStandardOutput("(begin (import (scheme write)) (call-with-current-continuation (lambda (k) (with-exception-handler "
				+ "(lambda (x) (display \"condition: \") (write x) (flush-output-port) (k 'exception)) "
				+ "(lambda () (+ 1 (raise 'an-error)))))))","condition: |an-error|");
		assertStandardOutput("(with-exception-handler (lambda (x) (write-string \"something went wrong\") (flush-output-port)) "
				+ "(lambda () (+ 1 (raise 'an-error))))","something went wrong");
	}
	@Test
	public void testEnvironment(){
		assertExpressionValue("(let () (import (scheme eval) (scheme repl)) (eval '(car (cons 1 2)) (interaction-environment)))","1");
		assertExpressionValue("(let () (import (scheme eval) (scheme repl)) (eval '(car (cons 1 2)) (environment '(scheme base))))","1");
		expectException("(let () (import (scheme eval) (scheme repl)) (eval '(car (cons 1 2)) (environment)))");
		assertExpressionValue("(let () (import (scheme eval) (scheme r5rs)) (eval '(exact->inexact 7) (scheme-report-environment 5)))","7.0");
		assertExpressionValue("(let () (import (scheme eval) (scheme r5rs)) (eval '(inexact->exact 7) (scheme-report-environment 5)))","7");
		assertExpressionValue("(begin (import (scheme r5rs)) (let ((f (eval '(lambda (f x) (f x x)) (null-environment 5)))) "
				+ "(f + 10)))","20");
	}
	@Test
	public void testIO(){
		assertExpressionValue("(input-port-open? (current-input-port))","#t");
		assertExpressionValue("(port? (current-input-port))","#t");
		assertExpressionValue("(input-port? (current-input-port))","#t");
		assertExpressionValue("(output-port? (current-input-port))","#f");
		assertExpressionValue("(textual-port? (current-input-port))","#t");
		assertExpressionValue("(binary-port? (current-input-port))","#f");
		assertExpressionValue("(port? (current-output-port))","#t");
		assertExpressionValue("(port? (current-error-port))","#t");
		assertExpressionValue("(output-port-open? (current-error-port))","#t");
		assertExpressionValue("(input-port? (current-output-port))","#f");
		assertExpressionValue("(output-port? (current-output-port))","#t");
		assertExpressionValue("(textual-port? (current-output-port))","#t");
		assertExpressionValue("(binary-port? (current-output-port))","#f");
		assertExpressionValue("(let ((port (open-input-string \" (hello '(#())) 4\"))) (import (scheme read)) "
				+ "(read port))","'(hello '(#()))");
		assertExpressionValue("(let ((port (open-input-string \" (hello '(#())) 4\"))) "
				+ "(read-char port))","#\\space");
		assertExpressionValue("(let* ((port (open-input-string \" (hello '(#())) 4\")) (x (peek-char port))) "
				+ "(eqv? (read-char port) x))","#t");
		assertExpressionValue("(let ((port (open-input-string \"hello \nworld\"))) "
				+ "(read-line port))","\"hello \"");
		assertExpressionValue("(let ((port (open-input-string \"hello \nworld\"))) "
				+ "(read-line port) (read-line port) (eof-object? (read-line port)))","#t");
		assertExpressionValue("(eof-object? (eof-object))","#t");
		assertExpressionValue("(eof-object? '())","#f");
		assertExpressionValue("(eof-object? \"\")","#f");
		assertExpressionValue("(let ((port (open-input-string \"hello \nworld\"))) "
				+ "(char-ready? port))","#t");
		assertExpressionValue("(let ((port (open-input-string \"\"))) "
				+ "(char-ready? port))","#t");
		assertExpressionValue("(let ((port (open-input-string \"hello \nworld\"))) "
				+ "(read-string 4 port))","\"hell\"");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19)))) "
				+ "(read-u8 port))","255");
		assertExpressionValue("(let* ((port (open-input-bytevector #u8(255 0 19))) (x (peek-u8 port))) "
				+ "(eqv? (read-u8 port) x))","#t");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19)))) "
				+ "(read-bytevector 2 port))","#u8(255 0)");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19)))) "
				+ "(read-bytevector 5 port))","#u8(255 0 19)");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19))) (vec (make-bytevector 5 7))) "
				+ "(read-bytevector! vec port) vec)","#u8(255 0 19 7 7)");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19))) (vec (make-bytevector 2 7))) "
				+ "(read-bytevector! vec port) vec)","#u8(255 0)");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19))) (vec (make-bytevector 2 7))) "
				+ "(read-bytevector! vec port 1) vec)","#u8(7 255)");
		assertExpressionValue("(let ((port (open-input-bytevector #u8(255 0 19))) (vec (make-bytevector 2 7))) "
				+ "(read-bytevector! vec port 0 1) vec)","#u8(255 7)");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write 'hello port) (get-output-string port))","\"|hello|\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write \"hello\\\"world\\\\\" port) (get-output-string port))","\"\\\"hello\\\\\\\"world\\\\\\\\\\\"\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write #\\a port) (get-output-string port))","\"#\\\\a\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-shared 'hello port) (get-output-string port))","\"|hello|\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-shared \"hello\\\"world\\\\\" port) (get-output-string port))","\"\\\"hello\\\\\\\"world\\\\\\\\\\\"\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-shared #\\a port) (get-output-string port))","\"#\\\\a\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-shared '#1088=(#1088# . #1088#) port) (get-output-string port))","\"#1089=(#1089# . #1089#)\"");

		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-simple 'hello port) (get-output-string port))","\"|hello|\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-simple \"hello\\\"world\\\\\" port) (get-output-string port))","\"\\\"hello\\\\\\\"world\\\\\\\\\\\"\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(write-simple #\\a port) (get-output-string port))","\"#\\\\a\"");

		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(display 'hello port) (get-output-string port))","\"hello\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(display \"hello\\\"world\\\\\" port) (get-output-string port))","\"hello\\\"world\\\\\"");
		assertExpressionValue("(let ((port (open-output-string))) (import (scheme write)) "
				+ "(display #\\a port) (get-output-string port))","\"a\"");

		assertExpressionValue("(let ((port (open-output-string))) "
				+ "(newline port) (get-output-string port))","\"\n\"");
		assertExpressionValue("(let ((port (open-output-string))) "
				+ "(write-char #\\a port) (get-output-string port))","\"a\"");
		assertExpressionValue("(let ((port (open-output-string))) "
				+ "(write-char #\\a port) (write-char #\\b port) (get-output-string port))","\"ab\"");
		assertExpressionValue("(let ((port (open-output-string))) "
				+ "(write-string \"hello world\" port) (get-output-string port))","\"hello world\"");
		assertExpressionValue("(let ((port (open-output-string))) "
				+ "(write-string \"hello world\" port 6) (get-output-string port))","\"world\"");
		assertExpressionValue("(let ((port (open-output-string))) "
				+ "(write-string \"hello world\" port 6 8) (get-output-string port))","\"wo\"");
		assertExpressionValue("(let ((port (open-output-bytevector))) "
				+ "(write-u8 0 port) (write-u8 255 port) (write-u8 34 port) (get-output-bytevector port))","#u8(0 255 34)");
		assertExpressionValue("(let ((port (open-output-bytevector))) "
				+ "(write-bytevector #u8(0 255 29) port) (get-output-bytevector port))","#u8(0 255 29)");
		assertExpressionValue("(let ((port (open-output-bytevector))) "
				+ "(write-bytevector #u8(0 255 29) port 1) (get-output-bytevector port))","#u8(255 29)");
		assertExpressionValue("(let ((port (open-output-bytevector))) "
				+ "(write-bytevector #u8(0 255 29) port 1 2) (get-output-bytevector port))","#u8(255)");
		assertExpressionValue("(let ((port (open-output-string))) (call-with-port port (lambda (p) (write-string \"hello\" p))) "
				+ "(get-output-string port))","\"hello\"");
		assertExpressionValue("(let ((port (open-output-string))) (call-with-port port (lambda (p) (write-string \"hello\" p))) "
				+ "(output-port-open? port))","#f");
		assertExpressionValue("(begin (import (scheme file)) (let ((out (open-output-file \"temp.txt\"))) "
				+ "(write-string \"hello world\" out) (close-port out) "
				+ "(let* ((in (open-input-file \"temp.txt\")) (content (read-line in))) (close-port in) content)))","\"hello world\"");
		assertExpressionValue("(begin (import (scheme file)) "
				+ "(call-with-output-file \"temp.txt\" (lambda (port) (write-string \"hello world\" port))) "
				+ "(call-with-input-file \"temp.txt\" (lambda (port) (read-line port))))","\"hello world\"");
		assertExpressionValue("(begin (import (scheme file)) (let ((out (open-binary-output-file \"temp.bin\"))) "
				+ "(write-u8 19 out) (close-port out) "
				+ "(let* ((in (open-binary-input-file \"temp.bin\")) (content (read-u8 in))) (close-port in) content)))","19");
		assertExpressionValue("(begin (import (scheme file)) (with-output-to-file \"temp.txt\" (lambda () (write-string \"hello cat\")))"
				+ "(with-input-from-file \"temp.txt\" (lambda () (read-line))))","\"hello cat\"");
		assertExpressionValue("(begin (import (scheme file)) (file-delete \"temp.txt\") (file-exists? \"temp.txt\"))","#f");
		assertExpressionValue("(begin (import (scheme file)) (file-delete \"temp.bin\") (file-exists? \"temp.bin\"))","#f");
		assertExpressionValue("(begin (close-port (current-error-port)) (output-port-open? (current-error-port)))","#f");
		assertExpressionValue("(begin (close-input-port (current-input-port)) (input-port-open? (current-input-port)))","#f");
		assertExpressionValue("(begin (close-output-port (current-output-port)) (output-port-open? (current-output-port)))","#f");
	}
	@Test
	public void testSystem(){
		assertStandardOutput("(begin (import (scheme load)) (load \"test/com/github/chungkwong/jschememin/lib-example.scm\"))",
				"   ** \n     *\n  *  *\n **  *\n    * \n" +
				" *    \n*  *  \n ** * \n **** \n      \n" +
				"      \n   *  \n    * \n *  * \n      \n");
		assertExpressionValue("(let () (import (scheme file)) (file-exists? \"test/com/github/chungkwong/jschememin/to_include.scm\"))","#t");
		assertExpressionValue("(let () (import (scheme file)) (file-exists? \"test/com/github/chungkwong/jschememin/nothing.scm\"))","#f");
		assertStandardOutput("(with-exception-handler "
				+ "(lambda (x) (import (scheme write)) (display (file-error? x)) (flush-output-port)) "
				+ "(lambda () (import (scheme file)) (file-delete \"test/com/github/chungkwong/jschememin/nothing.scm\")))" ,
				"#t");
		Main.main(new String[]{});
		System.err.println(Main.COMMAND_LINE);
		assertExpressionValue("(let () (import (scheme process-context)) (command-line))","'()");
		Main.main(new String[]{"hello","world"});
		assertExpressionValue("(let () (import (scheme process-context)) (command-line))","'(\"hello\" \"world\")");
		assertExpressionValue("(let () (import (scheme process-context)) (string? (get-environment-variable \"PATH\")))","#t");
		assertExpressionValue("(let () (import (scheme process-context)) (get-environment-variable \"NOTHING\"))","#f");
		assertExpressionValue("(let () (import (scheme process-context)) (list? (get-environment-variables)))","#t");
		assertExpressionValue("(let () (import (scheme process-context)) (let ((fst (car (get-environment-variables)))) (pair? fst)))","#t");
		assertExpressionValue("(let () (import (scheme process-context)) (let ((fst (car (get-environment-variables)))) (string? (car fst))))","#t");
		assertExpressionValue("(let () (import (scheme process-context)) (let ((fst (car (get-environment-variables)))) (string? (cdr fst))))","#t");
		assertExpressionValue("(let () (import (scheme time)) (integer? (current-second)))","#t");
		assertExpressionValue("(let () (import (scheme time)) (integer? (current-jiffy)))","#t");
		assertExpressionValue("(let () (import (scheme time)) (integer? (jiffies-per-second)))","#t");
		assertExpressionValue("(list? (features))","#t");
		assertExpressionValue("(symbol? (car (features)))","#t");
		assertExpressionValue("(pair? (member 'r7rs (features)))","#t");
		assertExpressionValue("(pair? (member 'jvm (features)))","#t");
	}
}