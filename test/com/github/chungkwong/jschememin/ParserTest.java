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
import java.util.*;
import org.junit.*;

/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ParserTest{
	public ParserTest(){
	}
	void check(String expr,ScmObject... obj){
		Assert.assertEquals(new Parser(expr).getRemainingDatums(),Arrays.asList(obj));
	}
	void expectSyntaxException(String expr){
		try{
			new Parser(expr).getRemainingDatums();
			Assert.assertTrue(false);
		}catch(SyntaxException ex){

		}
	}
	@Test
	public void testSimpleDatum(){
		check("#t",ScmBoolean.TRUE);
		check("1",ScmInteger.ONE);
		check("#\\a",new ScmCharacter('a'));
		check("hello",new ScmSymbol("hello"));
		check("\"hello\"",new ScmString("hello"));
		check("#u8(0 255 30)",new ScmByteVector(new byte[]{(byte)0,(byte)255,(byte)30}));
		check("#u8()",new ScmByteVector(new byte[]{}));
		expectSyntaxException("#u8(-1)");
		expectSyntaxException("#u8(2.5)");
		expectSyntaxException("#u8(256)");
		expectSyntaxException("#u8(1 . 2)");
	}
	@Test
	public void testCompoundDatum(){
		check("'a",ScmPair.toList(new ScmSymbol("quote"),new ScmSymbol("a")));
		check("`a",ScmPair.toList(new ScmSymbol("quasiquote"),new ScmSymbol("a")));
		check(",a",ScmPair.toList(new ScmSymbol("unquote"),new ScmSymbol("a")));
		check(",@a",ScmPair.toList(new ScmSymbol("unquote-splicing"),new ScmSymbol("a")));
		check("()",ScmNil.NIL);
		check("(a)",ScmPair.toList(new ScmSymbol("a")));
		check("(a b)",ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b")));
		check("(a b c)",ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b"),new ScmSymbol("c")));
		check("(())",ScmPair.toList(ScmNil.NIL));
		check("((a))",ScmPair.toList(ScmPair.toList(new ScmSymbol("a"))));
		check("((a) b)",ScmPair.toList(ScmPair.toList(new ScmSymbol("a")),new ScmSymbol("b")));
		check("((a b))",ScmPair.toList(ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b"))));
		check("(c (a b))",ScmPair.toList(new ScmSymbol("c"),ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b"))));
		check("(c (a b) ())",ScmPair.toList(new ScmSymbol("c"),ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b")),ScmNil.NIL));
		check("#()",ScmVector.toVector());
		check("#(a)",ScmVector.toVector(new ScmSymbol("a")));
		check("#(a b)",ScmVector.toVector(new ScmSymbol("a"),new ScmSymbol("b")));
		check("#(a b c)",ScmVector.toVector(new ScmSymbol("a"),new ScmSymbol("b"),new ScmSymbol("c")));
		check("#(#())",ScmVector.toVector(ScmVector.toVector()));
		check("#(())",ScmVector.toVector(ScmNil.NIL));
		check("(#())",ScmPair.toList(ScmVector.toVector()));
		check("#(#(a))",ScmVector.toVector(ScmVector.toVector(new ScmSymbol("a"))));
		check("#(#(a) b)",ScmVector.toVector(ScmVector.toVector(new ScmSymbol("a")),new ScmSymbol("b")));
		check("#(#(a b))",ScmVector.toVector(ScmVector.toVector(new ScmSymbol("a"),new ScmSymbol("b"))));
		check("#(c (a b))",ScmVector.toVector(new ScmSymbol("c"),ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b"))));
		check("#(c #(a b) ())",ScmVector.toVector(new ScmSymbol("c"),ScmVector.toVector(new ScmSymbol("a"),new ScmSymbol("b")),ScmNil.NIL));
		check("'()",ScmPair.toList(new ScmSymbol("quote"),ScmNil.NIL));
		check("'(a (b))",ScmPair.toList(new ScmSymbol("quote"),ScmPair.toList(new ScmSymbol("a"),ScmPair.toList(new ScmSymbol("b")))));
		check("'((b) a)",ScmPair.toList(new ScmSymbol("quote"),ScmPair.toList(ScmPair.toList(new ScmSymbol("b")),new ScmSymbol("a"))));
		check("('a)",ScmPair.toList(ScmPair.toList(new ScmSymbol("quote"),new ScmSymbol("a"))));
		check("(() 'a)",ScmPair.toList(ScmNil.NIL,ScmPair.toList(new ScmSymbol("quote"),new ScmSymbol("a"))));
		check("('a #(b c))",ScmPair.toList(ScmPair.toList(new ScmSymbol("quote"),new ScmSymbol("a")),ScmVector.toVector(new ScmSymbol("b"),new ScmSymbol("c"))));
	}
	@Test
	public void testDatumComment(){
		check("#;#t");
		check("#;1");
		check("#;#\\a");
		check("#;hello");
		check("#;\"hello\"");
		check("#;#u8(0 255 30)");
		check("#;(1 2 (3 4 ()) 5)");
		check("#;#10=(1 2 #10# 5)");
		check("#;#(1 2 '(3 4) 5)");
		check("#;#t 0",ScmInteger.ZERO);
		check("0 #;1",ScmInteger.ZERO);
		check("0 #;#\\a 0",ScmInteger.ZERO,ScmInteger.ZERO);
		check("(#;hello)",ScmNil.NIL);
		check("( (a) #;\"hello\" b)",ScmPair.toList(ScmPair.toList(new ScmSymbol("a")),new ScmSymbol("b")));
		check("((a #;#u8(0 255 30)))",ScmPair.toList(ScmPair.toList(new ScmSymbol("a"))));
		check("((#;(1 2 (3 4 ()) 5) a))",ScmPair.toList(ScmPair.toList(new ScmSymbol("a"))));
		check("((a #;#10=(1 2 #10# 5) b))",ScmPair.toList(ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b"))));
	}
	@Test
	public void testLabel(){
		check("#1=#t #2=#f (#1#)",ScmBoolean.TRUE,ScmBoolean.FALSE,ScmPair.toList(ScmBoolean.TRUE));
		check("#1=#t #1# #1#",ScmBoolean.TRUE,ScmBoolean.TRUE,ScmBoolean.TRUE);
		check("#1=(a b) #1#",ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b")),ScmPair.toList(new ScmSymbol("a"),new ScmSymbol("b")));
		ScmObject obj=new Parser("#1=(#1#)").nextDatum();
		Assert.assertTrue(obj instanceof ScmPair&&((ScmPair)obj).getCar()==obj);
		obj=new Parser("#1=(() . #1#)").nextDatum();
		Assert.assertTrue(obj instanceof ScmPair&&((ScmPair)obj).getCdr().equals(obj));
		obj=new Parser("#1=#(#1#)").nextDatum();
		Assert.assertTrue(obj instanceof ScmVector&&((ScmVector)obj).get(0).equals(obj));
		obj=new Parser("#1=#(1 #1# 4)").nextDatum();
		Assert.assertTrue(obj instanceof ScmVector&&((ScmVector)obj).get(1).equals(obj));
		obj=new Parser("#(#11=(#11#) #11#)").nextDatum();
		Assert.assertTrue(obj instanceof ScmVector&&((ScmVector)obj).get(0).equals(((ScmVector)obj).get(1)));
		Assert.assertTrue(((ScmVector)obj).get(0).equals(((ScmPair)((ScmVector)obj).get(0)).getCar()));
		expectSyntaxException("#1=3 #20#");
		expectSyntaxException("#1=#t #1=#t");
		expectSyntaxException("#1=(1 #1=a)");
	}
}