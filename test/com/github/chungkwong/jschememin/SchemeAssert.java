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
public class SchemeAssert{
	public static void assertExpressionValue(String expr,String result){
		ScmObject gotval=new Evaluator(true).eval(new Parser(expr).nextDatum());
		ScmObject expectval=new Evaluator(true).eval(new Parser(result).nextDatum());
		Assert.assertEquals(gotval,expectval);
	}
	public static void assertStandardOutput(String expr,String result){
		StringWriter out=new StringWriter();
		ScmPort.CURRENT_OUTPUT=new ScmTextualOutputPort(out);
		try{
			new Evaluator(true).eval(new Parser(expr).nextDatum());
		}catch(RuntimeException ex){

		}
		Assert.assertEquals(out.toString(),result);
	}
	public static void expectException(String expr){
		try{
			new Evaluator(true).eval(new Parser(expr).nextDatum());
			Assert.assertTrue(false);
		}catch(Throwable ex){

		}
	}
}
