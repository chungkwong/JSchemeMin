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
import org.junit.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class JavaInteractionTest{
	public static String sf;
	public String f;
	@Test
	public void test(){
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
	}
}
