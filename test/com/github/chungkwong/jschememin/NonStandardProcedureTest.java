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
import static com.github.chungkwong.jschememin.SchemeAssert.expectException;import org.junit.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class NonStandardProcedureTest{
	public static String sf;
	public String f;
	public static String overloaded(String a,Object b){
		return "0";
	}
	public static String overloaded(Object a,String b){
		return "1";
	}
	@Test
	public void testJavaInteraction(){
		assertExpressionValue("(begin (import (java)) (String->string (invoke (string->String \"  hello\n\") 'trim)))","\"hello\"");
		assertExpressionValue("(begin (import (java)) (String->symbol (invoke (symbol->String 'hello) 'substring (integer->Integer 2))))","'llo");
		assertExpressionValue("(begin (import (java)) (String->string (invoke-static 'java.lang.String 'join (string->String \",\"))))","\"\"");
		assertExpressionValue("(begin (import (java)) (String->string (invoke-static 'java.lang.String 'join (string->String \",\") (string->String \"hello\"))))","\"hello\"");
		assertExpressionValue("(begin (import (java)) (String->string (invoke-static 'java.lang.String 'join (string->String \",\") (string->String \"hello\") (string->String \"world\"))))","\"hello,world\"");
		assertExpressionValue("(begin (import (java)) (String->string (invoke-static 'java.lang.System 'getProperty (string->String \"Hello\") (string->String \"World\"))))","\"World\"");
		assertExpressionValue("(begin (import (java)) (+ 2 (Integer->integer (integer->Integer 4))))","6");
		assertExpressionValue("(begin (import (java)) (+ 2 (BigInteger->integer (integer->BigInteger 4))))","6");
		assertExpressionValue("(begin (import (java)) (+ 2 (Double->real (real->Double 4.5))))","6.5");
		assertExpressionValue("(begin (import (java)) (+ 2 (BigDecimal->real (real->BigDecimal 4.5))))","6.5");
		assertExpressionValue("(begin (import (java)) (ByteArray->bytevector (bytevector->ByteArray #u8(0 12 255))))","#u8(0 12 255)");
		assertExpressionValue("(begin (import (java)) (Boolean->boolean (boolean->Boolean #f)))","#f");
		assertExpressionValue("(begin (import (java)) (Boolean->boolean (invoke-static 'java.lang.Boolean 'logicalXor (boolean->Boolean #f) (boolean->Boolean #f))))","#f");
		assertExpressionValue("(begin (import (java)) (Boolean->boolean (invoke-static 'java.lang.Boolean 'logicalXor (boolean->Boolean #f) (boolean->Boolean #t))))","#t");
		assertExpressionValue("(begin (import (java)) (Boolean->boolean (invoke-static 'java.lang.Boolean 'logicalXor (boolean->Boolean #t) (boolean->Boolean #t))))","#f");
		assertExpressionValue("(begin (import (java)) (instanceof (string->String \"hello\") 'java.lang.String))","#t");
		assertExpressionValue("(begin (import (java)) (instanceof (string->String \"hello\") 'java.lang.Object))","#t");
		assertExpressionValue("(begin (import (java)) (instanceof (string->String \"hello\") 'java.lang.Integer))","#f");
		assertExpressionValue("(begin (import (java)) (Integer->integer (invoke (construct 'java.util.ArrayList (integer->Integer 5)) 'size)))","0");
		assertExpressionValue("(begin (import (java)) "
				+ "(set-static! 'com.github.chungkwong.jschememin.JavaInteractionTest 'sf (string->String \"world\")) "
				+ "(String->string (get-static 'com.github.chungkwong.jschememin.JavaInteractionTest 'sf)))","\"world\"");
		assertExpressionValue("(begin (import (java)) (let ((obj (construct 'com.github.chungkwong.jschememin.JavaInteractionTest)))"
				+ "(set! obj 'f (string->String \"world\")) "
				+ "(String->string (get obj 'f))))","\"world\"");
		assertExpressionValue("(begin (import (java)) (Boolean->boolean (invoke-static 'java.util.Objects 'isNull (null))))","#t");
		assertExpressionValue("(begin (import (java)) (String->string (invoke-static 'com.github.chungkwong.jschememin.JavaInteractionTest "
				+ "'overloaded (string->String \"q\") (integer->Integer -5))))","\"0\"");
		assertExpressionValue("(begin (import (java)) (String->string (invoke-static 'com.github.chungkwong.jschememin.JavaInteractionTest "
				+ "'overloaded (integer->Integer -5) (string->String \"q\"))))","\"1\"");
		assertExpressionValue("(begin (import (java)) (Integer->integer (invoke-static 'java.lang.Math 'abs (integer->Integer -5))))","5");
		expectException("(begin (import (java)) (invoke-static 'java.lang.math 'abs (integer->Integer -5)))");
		expectException("(begin (import (java)) (invoke-static 'java.lang.Math 'abs (string->String \"hello\")))");
		expectException("(begin (import (java)) (invoke 'java.lang.Math 'abs))");
		expectException("(begin (import (java)) (construct 'java.lang.Math (integer->Integer -5)))");
		expectException("(begin (import (java)) (invoke-static 'com.github.chungkwong.jschememin.JavaInteractionTest "
				+ "'overloaded (integer->Integer -5) (integer->Integer -5)))");
		expectException("(begin (import (java)) (invoke-static 'com.github.chungkwong.jschememin.JavaInteractionTest "
				+ "'overloaded (string->String \"q\") (string->String \"w\")))");
	}
	@Test
	public void testHashtable(){
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? '()))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? #(1 2 3)))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? (make-eq-hashtable)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? (make-eqv-hashtable)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? (make-hashtable equal-hash equal?)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-mutable? (hashtable-copy (make-eq-hashtable))))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-mutable? (hashtable-copy (make-eq-hashtable) #t)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-mutable? (hashtable-copy (make-eq-hashtable) #f)))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-mutable? (make-eq-hashtable)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-mutable? (make-eqv-hashtable)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-mutable? (make-hashtable equal-hash equal?)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? (make-eq-hashtable 4)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? (make-eqv-hashtable 4)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable? (make-hashtable equal-hash equal? 4)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-equivalence-function (make-eq-hashtable)))","eq?");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-equivalence-function (make-eqv-hashtable)))","eqv?");
		assertExpressionValue("(begin (import (scheme hashtables)) (hashtable-equivalence-function (make-hashtable equal-hash equal?)))","equal?");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-size table)))","0");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-size table)))","2");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) (hashtable-size table)))","2");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(hashtable-ref table 'one #f)))","'ONE");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(hashtable-ref table 'ONE #f)))","#f");
		assertExpressionValue("(begin (import (scheme hashtables) (scheme char)) (let ((table (make-hashtable string-ci-hash string-ci=?))) "
				+ "(hashtable-set! table \"one\" 1) (hashtable-set! table \"two\" 2) (hashtable-set! table \"oNe\" 'ONE) "
				+ "(hashtable-ref table \"ONE\" #f)))","'ONE");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(hashtable-delete! table 'one) (hashtable-contains? table 'one)))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(hashtable-delete! table 'two) (hashtable-contains? table 'one)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(hashtable-delete! table 'rubblish) (hashtable-contains? table 'one)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'two 2) (hashtable-update! table 'two (lambda (x) (* x x)) 7)"
				+ "(hashtable-ref table 'two #f)))","4");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 2) (hashtable-update! table 'two (lambda (x) (* x x)) 7)"
				+ "(hashtable-ref table 'two #f)))","49");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(= (hashtable-size table) (hashtable-size (hashtable-copy table)))))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(eq? (hashtable-ref table 'one #f) (hashtable-ref (hashtable-copy table) 'one #f))))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (let* ((table (make-hashtable equal-hash equal?)) (table2 (hashtable-copy table #t))) "
				+ "(hashtable-set! table 'one 1) (= (hashtable-size table) (hashtable-size table2))))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (let* ((table (make-hashtable equal-hash equal?)) (table2 (hashtable-copy table #t))) "
				+ "(hashtable-set! table2 'one 1) (= (hashtable-size table) (hashtable-size table2))))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(or (equal? (hashtable-keys table) #(one two)) (equal? (hashtable-keys table) #(two one)))))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-set! table 'one 'ONE) "
				+ "(let-values (((k v) (hashtable-entries table))) (or "
				+ "(and (equal? k #(one two)) (equal? v #(ONE 2)))"
				+ "(and (equal? k #(two one)) (equal? v #(2 ONE)))))))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (let ((table (make-hashtable equal-hash equal?))) "
				+ "(hashtable-set! table 'one 1) (hashtable-set! table 'two 2) (hashtable-clear! table) (hashtable-size table)))","0");
		assertExpressionValue("(begin (import (scheme hashtables)) (eqv? (equal-hash '(a b)) (equal-hash '(a b))))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (eqv? (symbol-hash 'hello) (equal-hash 'hello)))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (eqv? (string-hash \"hello\") (string-hash \"helLo\")))","#f");
		assertExpressionValue("(begin (import (scheme hashtables)) (eqv? (string-hash \"hello\") (string-hash \"hello\")))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (eqv? (string-ci-hash \"hello\") (string-ci-hash \"helLo\")))","#t");
		assertExpressionValue("(begin (import (scheme hashtables)) (eqv? (string-ci-hash \"hello\") (string-ci-hash \"heLo\")))","#f");
	}
	@Test
	public void testProfiler(){
		String fib=" (define fib (profile-lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) ";
		assertExpressionValue("(begin (import (jschememin) (scheme hashtables))"+fib+"(fib 2) (fib 4) (fib 1) (hashtable-size (profile-records)))","1");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 1))","1");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 2))","2");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 3))","3");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 4))","5");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 1) (> (total-time (profile-record fib)) 0))","#t");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 1) (count (profile-record fib)))","1");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 2) (count (profile-record fib)))","3");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 4) (count (profile-record fib)))","9");
		assertExpressionValue("(begin (import (jschememin))"+fib+"(fib 2) (fib 4) (fib 1) (count (profile-record fib)))","13");
		assertExpressionValue("(begin (import (jschememin)) (> (duration (+ 2 3)) 0))","#t");
		assertExpressionValue("(begin (import (jschememin)) (> (memory-free) 0))","#t");
		assertExpressionValue("(begin (import (jschememin)) (> (memory-total) 0))","#t");
		assertExpressionValue("(begin (import (jschememin)) (< (memory-free) (memory-total)))","#t");
	}
}