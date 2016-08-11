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
		assertExpressionValue("(append '(a b) '(c . d))","'(a b c . d)");
		assertExpressionValue("(append '() 'a)","'a");
		assertExpressionValue("(reverse '(a b c))","'(c b a)");
		assertExpressionValue("(reverse '(a (b c) d (e (f))))","'((e (f)) d (b c) a)");
		assertExpressionValue("(list-ref '(a b c d) 2)","'c");
		assertExpressionValue("(list-ref '(a b c d) (exact (round 1.8)))","'c");
	}
	@Test
	public void testSymbol(){
		assertExpressionValue("(symbol? 'foo)","#t");
		assertExpressionValue("(symbol? (car '(a b)))","#t");
		assertExpressionValue("(symbol? \"bar\")","#f");
		assertExpressionValue("(symbol? 'nil)","#t");
		assertExpressionValue("(symbol? '())","#f");
		assertExpressionValue("(symbol? #f)","#f");
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
	}
	@Test
	public void testVector(){
	}
	@Test
	public void testByteVector(){
		assertExpressionValue("(make-bytevector 2 12)","#u8(12 12)");
		assertExpressionValue("(bytevector 1 3 5 1 3 5)","#u8(1 3 5 1 3 5)");
		assertExpressionValue("(bytevector)","#u8()");
		assertExpressionValue("(bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5)","8");
		assertExpressionValue("(let ((bv (bytevector 1 2 3 4))) (bytevector-u8-set! bv 1 3) bv)","#u8(1 3 3 4)");
		assertExpressionValue("(bytevector-copy #u8(1 2 3 4 5) 2 4))","#u8(3 4)");
		assertExpressionValue("(let ((a (bytevector 1 2 3 4 5)) (b (bytevector 10 20 30 40 50))) (bytevector-copy! b 1 a 0 2) b)"
				,"#u8(10 1 2 40 50)%");
		assertExpressionValue("(bytevector-append #u8(0 1 2) #u8(3 4 5))","#u8(0 1 2 3 4 5)");
		assertExpressionValue("(utf8->string #u8(#x41))","\"A\"");
		assertExpressionValue("(string->utf8 \"λ\")","#u8(#xCE #xBB #x0)");
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
