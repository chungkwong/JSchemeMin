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
import com.github.chungkwong.jschememin.type.*;
import java.io.*;
import org.junit.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class StandardProcedureTest{
	public StandardProcedureTest(){
	}
	public void assertExpressionValue(String expr,String result){
		ScmObject gotval=new Evaluator(true).eval(new Parser(expr).nextDatum());
		ScmObject expectval=new Evaluator(true).eval(new Parser(result).nextDatum());
		Assert.assertEquals(gotval,expectval);
	}
	public void assertStandardOutput(String expr,String result){
		StringWriter out=new StringWriter();
		ScmPort.CURRENT_OUTPUT=new ScmTextualOutputPort(out);
		try{
			new Evaluator(true).eval(new Parser(expr).nextDatum());
		}catch(RuntimeException ex){

		}
		Assert.assertEquals(out.toString(),result);
	}
	void expectException(String expr){
		try{
			new Evaluator(true).eval(new Parser(expr).nextDatum());
			Assert.assertTrue(false);
		}catch(Throwable ex){

		}
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
		assertExpressionValue("(eq? 'a 'a)","#t");
		assertExpressionValue("(eq? (list 'a) (list 'a))","#f");
		assertExpressionValue("(eq? '() '())","#t");
		assertExpressionValue("(eq? car car)","#t");
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
		/*assertExpressionValue("(memq 'a '(a b c))","'(a b c)");
		assertExpressionValue("(memq 'b '(a b c))","'(b c)");
		assertExpressionValue("(memq 'a '(b c d))","#f");
		assertExpressionValue("(memq (list 'a) '(b (a) c))","#f");
		assertExpressionValue("(member (list 'a) '(b (a) c))","'((a) c)");
		assertExpressionValue("(member \"B\" '(\"a\" \"b\" \"c\") string-ci=?)","'(\"b\" \"c\")");
		assertExpressionValue("(memv 101 '(100 101 102))","'(101 102)");
		assertExpressionValue("(assq 'a '((a 1) (b 2) (c 3)))","'(a 1)");
		assertExpressionValue("(assq 'b '((a 1) (b 2) (c 3)))","'(b 2)");
		assertExpressionValue("(assq 'd '((a 1) (b 2) (c 3)))","#f");
		assertExpressionValue("(assq (list 'a) '(((a)) ((b)) ((c))))","#f");
		assertExpressionValue("(assoc (list 'a) '(((a)) ((b)) ((c))))","((a))");
		assertExpressionValue("(assoc 2.0 '((1 1) (2 4) (3 9)) =)","(2 4)");
		assertExpressionValue("(assv 5 '((2 3) (5 7) (11 13)))","(5 7)");*/
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
				"13");//TODO
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
		assertExpressionValue("(procedure? if)","#f");
		assertExpressionValue("(procedure? let)","#f");
		assertExpressionValue("(apply + (list 3 4))","7");
		assertExpressionValue("(apply + 1 (list 3 4))","8");
		assertExpressionValue("(let ((compose (lambda (f g) (lambda args (f (apply g args)))))) "
				+ "(import (scheme inexact) (scheme complex)) (< (magnitude (- ((compose sqrt *) 12 75) 30)) 1e-12))","#t");
		assertExpressionValue("(call-with-current-continuation (lambda (exit) (exit 7) #t))","7");
		assertExpressionValue("(call-with-values (lambda () (values 4 5)) (lambda (a b) b))","5");
		assertExpressionValue("(call-with-values * -)","-1");
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
	}
	@Test
	public void testEnvironment(){
		assertExpressionValue("(let () (import (scheme eval) (scheme repl)) (eval '(car (cons 1 2)) (interaction-environment)))","1");
		assertExpressionValue("(let () (import (scheme eval) (scheme repl)) (eval '(car (cons 1 2)) (environment '(scheme base))))","1");
		expectException("(let () (import (scheme eval) (scheme repl)) (eval '(car (cons 1 2)) (environment)))");
	}
	@Test
	public void testIO(){
	}
	@Test
	public void testSystem(){
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
		assertExpressionValue("(string? (car (features)))","#t");

	}
}