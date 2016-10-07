/*
 * Copyright (C) 2016 Chan Chung Kwong
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 */
package com.github.chungkwong.jschememin;

import com.github.chungkwong.jschememin.type.*;
import java.io.*;
import java.math.*;
import java.util.*;
import org.junit.*;

/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class LexTest{

	public LexTest(){
	}
	private static void assertListEqual(List a,List b){
		Assert.assertEquals(a.size(),b.size());
		Iterator ia=a.iterator(),ib=b.iterator();
			while(ia.hasNext())
				Assert.assertEquals(ia.next(),ib.next());
	}
	private static void assertComplexEqualSpecial(String expr,ScmComplex b){
		ScmComplex a=(ScmComplex)new Lex(expr).nextToken();
		Assert.assertEquals(a.getReal(),b.getReal());
		Assert.assertSame(a.getImag(),b.getImag());
	}
	private static void check(String expr,Object... tokens){
		List result=null;
		try{
			result=new Lex(expr).getRemainingTokens();
		}catch(IOException ex){
			Assert.assertTrue(false);
		}
		assertListEqual(result,Arrays.asList(tokens));
	}
	@Test
	public void testWhitespace(){
		check("");
		check("\n");
		check("\nhello",new ScmSymbol("hello"));
		check("\r");
		check("\r\n");
		check(" ");
		check("\t");
		check(";dnwfke");
		check(";dnwfke\n");
		check(";dnwfke\n#t",ScmBoolean.TRUE);
		check("#|;dnwfke|#");
		check("#|;dn#||#wfke|#");
		check("#|;dn#|ds|g#r|#wfke|#");
		check("#|;dn##|ds|g##r||#wfke|#");
		check("#|;dn#|ds|g#r|#wfke|#hello",new ScmSymbol("hello"));
	}
    @Test
	public void testBoolean(){
		check("#t",ScmBoolean.TRUE);
		check("#true",ScmBoolean.TRUE);
		check("#True",ScmBoolean.TRUE);
		check("#tRue",ScmBoolean.TRUE);
		check("#trUe",ScmBoolean.TRUE);
		check("#TRUE",ScmBoolean.TRUE);
		check("#f",ScmBoolean.FALSE);
		check("#false",ScmBoolean.FALSE);
		check("#False",ScmBoolean.FALSE);
		check("#fAlse",ScmBoolean.FALSE);
		check("#faLse",ScmBoolean.FALSE);
		check("#FALSE",ScmBoolean.FALSE);
	}
	@Test
	public void testCharacter(){
		check("#\\ ",ScmCharacter.getScmCharacter(' '));
		check("#\\a",ScmCharacter.getScmCharacter('a'));
		check("#\\x",ScmCharacter.getScmCharacter('x'));
		check("#\\x4F",ScmCharacter.getScmCharacter('\u004f'));
		check("#\\alarm",ScmCharacter.getScmCharacter('\u0007'));
		check("#\\backspace",ScmCharacter.getScmCharacter('\u0008'));
		check("#\\delete",ScmCharacter.getScmCharacter('\u007f'));
		check("#\\escape",ScmCharacter.getScmCharacter('\u001b'));
		check("#\\newline",ScmCharacter.getScmCharacter('\n'));
		check("#\\null",ScmCharacter.getScmCharacter(0));
		check("#\\return",ScmCharacter.getScmCharacter('\r'));
		check("#\\space",ScmCharacter.getScmCharacter(' '));
		check("#\\tab",ScmCharacter.getScmCharacter('\t'));
		check("#\\a)",ScmCharacter.getScmCharacter('a'),SimpleToken.getToken(")"));
		check("#\\ )",ScmCharacter.getScmCharacter(' '),SimpleToken.getToken(")"));
		check("#\\x)",ScmCharacter.getScmCharacter('x'),SimpleToken.getToken(")"));
		check("#\\x4F)",ScmCharacter.getScmCharacter('\u004f'),SimpleToken.getToken(")"));
		check("#\\alarm)",ScmCharacter.getScmCharacter('\u0007'),SimpleToken.getToken(")"));
	}
	@Test
	public void testRealNumber(){
		check("0",new ScmInteger(BigInteger.valueOf(0)));
		check("13",new ScmInteger(BigInteger.valueOf(13)));
		check("#b1101",new ScmInteger(BigInteger.valueOf(13)));
		check("#o15",new ScmInteger(BigInteger.valueOf(13)));
		check("#d13",new ScmInteger(BigInteger.valueOf(13)));
		check("#xD",new ScmInteger(BigInteger.valueOf(13)));
		check("#I#B1101",new ScmFloatingPointNumber(new BigDecimal(13)));
		check("#O#i15",new ScmFloatingPointNumber(new BigDecimal(13)));
		check("#E#D13",new ScmInteger(BigInteger.valueOf(13)));
		check("#X#eD",new ScmInteger(BigInteger.valueOf(13)));
		check("-0",new ScmInteger(BigInteger.valueOf(0)));
		check("-13",new ScmInteger(BigInteger.valueOf(-13)));
		check("#b-1101",new ScmInteger(BigInteger.valueOf(-13)));
		check("#o-15",new ScmInteger(BigInteger.valueOf(-13)));
		check("#d-13",new ScmInteger(BigInteger.valueOf(-13)));
		check("#x-D",new ScmInteger(BigInteger.valueOf(-13)));
		check("#I#B-1101",new ScmFloatingPointNumber(new BigDecimal(-13)));
		check("#O#i-15",new ScmFloatingPointNumber(new BigDecimal(-13)));
		check("#E#D-13",new ScmInteger(BigInteger.valueOf(-13)));
		check("#X#e-D",new ScmInteger(BigInteger.valueOf(-13)));
		check("4/2",new ScmRational(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(1))));
		check("-4/2",new ScmRational(new ScmInteger(BigInteger.valueOf(-2)),new ScmInteger(BigInteger.valueOf(1))));
		check("#b1101/11",new ScmRational(new ScmInteger(BigInteger.valueOf(13)),new ScmInteger(BigInteger.valueOf(3))));
		check("#x-D/3",new ScmRational(new ScmInteger(BigInteger.valueOf(-13)),new ScmInteger(BigInteger.valueOf(3))));
		check("3e2",new ScmFloatingPointNumber(new BigDecimal(300)));
		check("#e3e2",new ScmInteger(BigInteger.valueOf(300)));
		check("3.1e+2",new ScmFloatingPointNumber(new BigDecimal(310)));
		check("#e3100e-2",new ScmInteger(BigInteger.valueOf(31)));
		check("-3e2",new ScmFloatingPointNumber(new BigDecimal(-300)));
		check("#e-3e2",new ScmInteger(BigInteger.valueOf(-300)));
		check("#e-3.1e+2",new ScmInteger(BigInteger.valueOf(-310)));
		check("-3100e-2",new ScmFloatingPointNumber(new BigDecimal(-31)));
		check("4.",new ScmFloatingPointNumber(new BigDecimal(4)));
		check("-4.",new ScmFloatingPointNumber(new BigDecimal(-4)));
		check("#e4.",new ScmInteger(BigInteger.valueOf(4)));
		check("#e-4.",new ScmInteger(BigInteger.valueOf(-4)));
		check("23.670",new ScmFloatingPointNumber(new BigDecimal("23.67")));
		check("#e23.670",new ScmFloatingPointNumber(new BigDecimal("23.67")).toScmRational());
		check(".670",new ScmFloatingPointNumber(new BigDecimal(".67")));
		check("1.e-10",new ScmFloatingPointNumber(new BigDecimal("1e-10")));
		check("1.e+10",new ScmFloatingPointNumber(new BigDecimal("10000000000")));
		check("#e.5e10",new ScmInteger(new BigInteger("5000000000")));
		check("-23.670",new ScmFloatingPointNumber(new BigDecimal("-23.67")));
		check("-.670",new ScmFloatingPointNumber(new BigDecimal("-.67")));
		check("-1.e-10",new ScmFloatingPointNumber(new BigDecimal("-1e-10")));
		check("#e-1.e+10",new ScmInteger(new BigInteger("-10000000000")));
		check("-.5e10",new ScmFloatingPointNumber(new BigDecimal("-5000000000")));
		Assert.assertSame(new Lex("+nan.0").nextToken(),ScmSpecialReal.POSITIVE_NAN);
		Assert.assertSame(new Lex("-nan.0").nextToken(),ScmSpecialReal.POSITIVE_NAN);
		Assert.assertSame(new Lex("+inf.0").nextToken(),ScmSpecialReal.POSITIVE_INF);
		Assert.assertSame(new Lex("-inf.0").nextToken(),ScmSpecialReal.NEGATIVE_INF);
		check("0)",new ScmInteger(BigInteger.valueOf(0)),SimpleToken.getToken(")"));
		check("13)",new ScmInteger(BigInteger.valueOf(13)),SimpleToken.getToken(")"));
		check("#b1101)",new ScmInteger(BigInteger.valueOf(13)),SimpleToken.getToken(")"));
		check("#X#eD)",new ScmInteger(BigInteger.valueOf(13)),SimpleToken.getToken(")"));
		check("-13)",new ScmInteger(BigInteger.valueOf(-13)),SimpleToken.getToken(")"));
		check("#b-1101)",new ScmInteger(BigInteger.valueOf(-13)),SimpleToken.getToken(")"));
		check("4/2)",new ScmRational(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(1))),SimpleToken.getToken(")"));
		check("#b1101/11)",new ScmRational(new ScmInteger(BigInteger.valueOf(13)),new ScmInteger(BigInteger.valueOf(3))),SimpleToken.getToken(")"));
		check("3e2)",new ScmFloatingPointNumber(new BigDecimal(300)),SimpleToken.getToken(")"));
		check("23.670)",new ScmFloatingPointNumber(new BigDecimal("23.67")),SimpleToken.getToken(")"));
		check(".670)",new ScmFloatingPointNumber(new BigDecimal(".67")),SimpleToken.getToken(")"));
		check("1.e-10)",new ScmFloatingPointNumber(new BigDecimal("1e-10")),SimpleToken.getToken(")"));
		Assert.assertSame(new Lex("+nan.0)").nextToken(),ScmSpecialReal.POSITIVE_NAN);
		try{
			Assert.assertEquals(new Lex("+nan.0)").getRemainingTokens().size(),2);
			Assert.assertEquals(new Lex("+nan.0)").getRemainingTokens().get(1),SimpleToken.getToken(")"));
		}catch(IOException ex){
			Assert.assertTrue(false);
		}
	}
	@Test
	public void testComplexNumber(){
		check("+i",new ScmComplexRectangular(ScmInteger.ZERO,ScmInteger.ONE));
		check("-i",new ScmComplexRectangular(ScmInteger.ZERO,ScmInteger.ONE.negate()));
		check("+40i",new ScmComplexRectangular(ScmInteger.ZERO,new ScmInteger(BigInteger.valueOf(40))));
		check("-2.5e-1i",new ScmComplexRectangular(ScmInteger.ZERO,new ScmFloatingPointNumber(new BigDecimal(-0.25))));
		check("1/2-i",new ScmComplexRectangular(new ScmRational(ScmInteger.ONE,ScmInteger.ONE.add(ScmInteger.ONE)),ScmInteger.ONE.negate()));
		check("1+i",new ScmComplexRectangular(ScmInteger.ONE,ScmInteger.ONE));
		assertComplexEqualSpecial("1+nan.0i",new ScmComplexRectangular(ScmInteger.ONE,ScmSpecialReal.POSITIVE_NAN));
		assertComplexEqualSpecial("1-nan.0i",new ScmComplexRectangular(ScmInteger.ONE,ScmSpecialReal.POSITIVE_NAN));
		assertComplexEqualSpecial("1+inf.0i",new ScmComplexRectangular(ScmInteger.ONE,ScmSpecialReal.POSITIVE_INF));
		assertComplexEqualSpecial("1-inf.0i",new ScmComplexRectangular(ScmInteger.ONE,ScmSpecialReal.NEGATIVE_INF));
		check("2+3i",new ScmComplexRectangular(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(3))));
		check("#b1101+11i",new ScmComplexRectangular(new ScmInteger(BigInteger.valueOf(13)),new ScmInteger(BigInteger.valueOf(3))));
		check("#b1101-11i",new ScmComplexRectangular(new ScmInteger(BigInteger.valueOf(13)),new ScmInteger(BigInteger.valueOf(-3))));
		check("2-3i",new ScmComplexRectangular(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(-3))));
		check("2@3",new ScmComplexPolar(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(3))));
		check("2@-3",new ScmComplexPolar(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(-3))));
		assertComplexEqualSpecial("+nan.0i",new ScmComplexRectangular(ScmInteger.ZERO,ScmSpecialReal.POSITIVE_NAN));
		assertComplexEqualSpecial("-nan.0i",new ScmComplexRectangular(ScmInteger.ZERO,ScmSpecialReal.POSITIVE_NAN));
		assertComplexEqualSpecial("+inf.0i",new ScmComplexRectangular(ScmInteger.ZERO,ScmSpecialReal.POSITIVE_INF));
		assertComplexEqualSpecial("-inf.0i",new ScmComplexRectangular(ScmInteger.ZERO,ScmSpecialReal.NEGATIVE_INF));
		check("+i)",new ScmComplexRectangular(ScmInteger.ZERO,ScmInteger.ONE),SimpleToken.getToken(")"));
		check("+40i)",new ScmComplexRectangular(ScmInteger.ZERO,new ScmInteger(BigInteger.valueOf(40))),SimpleToken.getToken(")"));
		check("1+i)",new ScmComplexRectangular(ScmInteger.ONE,ScmInteger.ONE),SimpleToken.getToken(")"));
		assertComplexEqualSpecial("1+nan.0i)",new ScmComplexRectangular(ScmInteger.ONE,ScmSpecialReal.POSITIVE_NAN));
		check("2-3i)",new ScmComplexRectangular(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(-3))),SimpleToken.getToken(")"));
		check("2@-3)",new ScmComplexPolar(new ScmInteger(BigInteger.valueOf(2)),new ScmInteger(BigInteger.valueOf(-3))),SimpleToken.getToken(")"));
		assertComplexEqualSpecial("+nan.0i)",new ScmComplexRectangular(ScmInteger.ZERO,ScmSpecialReal.POSITIVE_NAN));
	}
	@Test
	public void testString(){
		check("\"\"",new ScmString(""));
		check("\"ab \"",new ScmString("ab "));
		check("\"\\aab\"",new ScmString("\u0007ab"));
		check("\"\\bab\"",new ScmString("\bab"));
		check("\"\\tab\"",new ScmString("\tab"));
		check("\"\\nab\"",new ScmString("\nab"));
		check("\"\\rab\"",new ScmString("\rab"));
		check("\"\\\"ab\"",new ScmString("\"ab"));
		check("\"\\\\\\\"\"",new ScmString("\\\""));
		check("\"\\x4f;ab\"",new ScmString("\u004fab"));
		check("\"hello\\ \t\n  world\"",new ScmString("helloworld"));
		check("\"ab \")",new ScmString("ab "),SimpleToken.getToken(")"));
	}
	@Test
	public void testIdentifier(){
		check("+",new ScmSymbol("+"));
		check("+@",new ScmSymbol("+@"));
		check("+-df",new ScmSymbol("+-df"));
		check("++",new ScmSymbol("++"));
		check("+.@",new ScmSymbol("+.@"));
		check("+.-df",new ScmSymbol("+.-df"));
		check("+..+",new ScmSymbol("+..+"));
		check("+a",new ScmSymbol("+a"));
		check("-",new ScmSymbol("-"));
		check("-@",new ScmSymbol("-@"));
		check("--df+-.@)",new ScmSymbol("--df+-.@"),SimpleToken.getToken(")"));
		check("-+",new ScmSymbol("-+"));
		check("..",new ScmSymbol(".."));
		check(".@",new ScmSymbol(".@"));
		check(".+",new ScmSymbol(".+"));
		check(".--d",new ScmSymbol(".--d"));
		check(".a",new ScmSymbol(".a"));
		check("||",new ScmSymbol(""));
		check("|hello\nworld|",new ScmSymbol("hello\nworld"));
		check("|hello\\|\\x4f;world|",new ScmSymbol("hello|\u004fworld"));
		check("abcxyz0129ABCXYZ!$%&*/:<=>?^_~+-.@)"
				,new ScmSymbol("abcxyz0129ABCXYZ!$%&*/:<=>?^_~+-.@"),SimpleToken.getToken(")"));
		check("!",new ScmSymbol("!"));
		check("$",new ScmSymbol("$"));
		check("%",new ScmSymbol("%"));
		check("&",new ScmSymbol("&"));
		check("*",new ScmSymbol("*"));
		check("/",new ScmSymbol("/"));
		check(":",new ScmSymbol(":"));
		check("<",new ScmSymbol("<"));
		check("=",new ScmSymbol("="));
		check(">",new ScmSymbol(">"));
		check("?",new ScmSymbol("?"));
		check("^",new ScmSymbol("^"));
		check("_",new ScmSymbol("_"));
		check("~",new ScmSymbol("~"));
		check("!",new ScmSymbol("!"));
		check("|H\\x65;llo\\|world|",new ScmSymbol("Hello|world"));
		check("|\\x3BB;|",new ScmSymbol("λ"));
		check("|\\t\\t|",new ScmSymbol("\u0009\u0009"));
		check("...",new ScmSymbol("..."));
		check("+soup+",new ScmSymbol("+soup+"));
		check("<=?",new ScmSymbol("<=?"));
		check("->string",new ScmSymbol("->string"));
		check("a34kTMNs",new ScmSymbol("a34kTMNs"));
		check("lambda",new ScmSymbol("lambda"));
		check("list->vector",new ScmSymbol("list->vector"));
		check("q",new ScmSymbol("q"));
		check("V17a",new ScmSymbol("V17a"));
		check("|two word|",new ScmSymbol("two word"));
		check("|two\\x20;word|",new ScmSymbol("two word"));
		check("the-word-recursion-has-many-meanings",new ScmSymbol("the-word-recursion-has-many-meanings"));
	}
	@Test
	public void testPunctuation(){
		check("(",SimpleToken.getToken("("));
		check(")",SimpleToken.getToken(")"));
		check("#(",SimpleToken.getToken("#("));
		check("#u8(",SimpleToken.getToken("#u8("));
		check("#U8(",SimpleToken.getToken("#u8("));
		check("\'",SimpleToken.getToken("\'"));
		check("`",SimpleToken.getToken("`"));
		check(",",SimpleToken.getToken(","));
		check(",@",SimpleToken.getToken(",@"));
		check(".",SimpleToken.getToken("."));
	}
	@Test
	public void testCaseFolding(){
		check("#!fold-case");
		check("#!no-fold-case");
	}
}
