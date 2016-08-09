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
	void expectException(String expr){
		try{
			new Evaluator(true).eval(new Parser(expr).nextDatum());
			Assert.assertTrue(false);
		}catch(Exception ex){

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
		assertExpressionValue("(complex? 3+4i)","#t");
		assertExpressionValue("(complex? 3)","#t");
		assertExpressionValue("(real? 3)","#t");
		assertExpressionValue("(real? -2.5+0i)","#t");
		assertExpressionValue("(real? -2.5+0.0i)","#f");
		assertExpressionValue("(real? \\#e1e10)","#t");
		assertExpressionValue("(real? +inf.0)","#t");
		assertExpressionValue("(real? +nan.0)","#t");
		assertExpressionValue("(rational? -inf.0)","#f");
		assertExpressionValue("(rational? 3.5)","#t");
		assertExpressionValue("(rational? 6/10)","#t");
		assertExpressionValue("(rational? 6/3)","#t");
		assertExpressionValue("(integer? 3+0i)","#t");
		assertExpressionValue("(integer? 3.0)","#t");
		assertExpressionValue("(integer? 8/4)","#t");
		assertExpressionValue("(exact? 3.0)","#f");
		assertExpressionValue("(exact? \\#e3.0)","#t");
		assertExpressionValue("(inexact? 3.)","#t");
		assertExpressionValue("(exact-integer? 32)","#t");
		assertExpressionValue("(exact-integer? 32.0)","#f");
		assertExpressionValue("(exact-integer? 32/5)","#f");
		assertExpressionValue("(finite? 3)","#t");
		assertExpressionValue("(finite? +inf.0)","#f");
		assertExpressionValue("(finite? 3.0+inf.0i)","#f");
		assertExpressionValue("(infinite? 3)","#f");
		assertExpressionValue("(infinite? +inf.0)","#t");
		assertExpressionValue("(infinite? +nan.0)","#f");
		assertExpressionValue("(infinite? 3.0+inf.0i)","#t");
		assertExpressionValue("(nan? +nan.0)","#t");
		assertExpressionValue("(nan? 32)","#f");
		assertExpressionValue("(nan? +nan.0+5.0i)","#t");
		assertExpressionValue("(nan? 1+2i)","#f");
		assertExpressionValue("(max 3 4)","4");
		assertExpressionValue("(max 3.9 4)","4.0");
		assertExpressionValue("(+ 3 4)","7");
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
		assertExpressionValue("(floor/ 5 2)","2 1");
		assertExpressionValue("(floor/ -5 2)","-3 1");
		assertExpressionValue("(floor/ 5 -2)","-3 -1");
		assertExpressionValue("(floor/ -5 -2)","2 -1");
		assertExpressionValue("(truncate/ 5 2)","2 1");
		assertExpressionValue("(truncate/ -5 2)","-2 -1");
		assertExpressionValue("(truncate/ 5 -2)","-2 1");
		assertExpressionValue("(truncate/ -5 -2)","2 -1");
		assertExpressionValue("(truncate/ -5.0 -2)","2.0 -1.0");
		assertExpressionValue("(gcd 32 -36)","4");
		assertExpressionValue("(gcd)","0");
		assertExpressionValue("(lcm 32 -36)","288");
		assertExpressionValue("(lcm 32.0 -36)","288.0");
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
		assertExpressionValue("(square 42)","1764");
		assertExpressionValue("(square 2.0)","4.0");
		assertExpressionValue("(sqrt 9)","3");
		assertExpressionValue("(sqrt -1)","+i");
		assertExpressionValue("(exact-integer-sqrt 4)","2 0");
		assertExpressionValue("(exact-integer-sqrt 5)","2 1");
		assertExpressionValue("(string->number \"100\")","100");
		assertExpressionValue("(string->number \"100\" 16)","256");
		assertExpressionValue("(string->number \"1e2\")","100.0");
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
		assertExpressionValue("(make-list 2 3)","'(3 3)");
		assertExpressionValue("(list 'a (+ 3 4) 'c)","'(a 7 c)");
		assertExpressionValue("(list)","'()");
		assertExpressionValue("(length '(a b c))","3");
		assertExpressionValue("(length '(a (b) (c d e)))","3");
		assertExpressionValue("(length '())","0");
		assertExpressionValue("(append '(x) '(y))","'(x y)");
		assertExpressionValue("(append '(a) '(b c d))","'(a b c d)");
		assertExpressionValue("(append '(a (b)) '((c)))","'(a (b) (c))");
		assertExpressionValue("(append '(a b) '(c . d))","(a b c . d)");
		assertExpressionValue("(append '() 'a)","'a");
		assertExpressionValue("(reverse '(a b c))","'(c b a)");
		assertExpressionValue("(reverse '(a (b c) d (e (f))))","'((e (f)) d (b c) a)");
		assertExpressionValue("(list-ref '(a b c d) 2)","'c");
		assertExpressionValue("(list-ref '(a b c d) (exact (round 1.8)))","'c");
	}
	@Test
	public void testSymbol(){
	}
	@Test
	public void testCharacter(){
	}
	@Test
	public void testString(){
	}
	@Test
	public void testVector(){
	}
	@Test
	public void testByteVector(){
	}
	@Test
	public void testControl(){
	}
	@Test
	public void testException(){
	}
	@Test
	public void testEnvironment(){
	}
	@Test
	public void testIO(){
	}
	@Test
	public void testSystem(){
	}
}
