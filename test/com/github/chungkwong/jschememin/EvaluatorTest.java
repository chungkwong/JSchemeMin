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
public class EvaluatorTest{
	void check(String expr,ScmObject... obj){
		Evaluator evaluator=new Evaluator(true);
		Assert.assertArrayEquals(new Parser(expr).getRemainingDatums().stream().map((d)->evaluator.eval(d)).toArray(),obj);
	}
	void checkLast(String expr,ScmObject obj){
		Evaluator evaluator=new Evaluator(true);
		Object[] result=new Parser(expr).getRemainingDatums().stream().map((d)->evaluator.eval(d)).toArray();
		Assert.assertEquals(result[result.length-1],obj);
	}
	void expectException(String expr){
		try{
			new Evaluator(true).eval(new Parser(expr).nextDatum());
			Assert.assertTrue(false);
		}catch(Exception ex){

		}
	}
	@Test
	public void testIf(){
		check("(if #f 'a 'b)",new ScmSymbol("b"));
		check("(if #t 'a 'b)",new ScmSymbol("a"));
		check("(if '() 'a 'b)",new ScmSymbol("a"));
	}
	@Test
	public void testLambda(){
		checkLast("(set! f (lambda () (set! x '()))) x",Environment.UNBOUNDED);
		checkLast("(set! x 3) (set! f (lambda () (set! x '()))) (f) x",ScmNil.NIL);
	}
	@Test
	public void testInclude(){
		check("(include \"/home/kwong/NetBeansProjects/JSchemeMin/test/com/github/chungkwong/jschememin/to_include.scm\")",new ScmInteger(12));
	}
	@Test
	public void testSet(){
		//check("(begin (* 2 3) (+ 4 5))",new ScmInteger(9));
	}
	@Test
	public void testImport(){
		checkLast("(import (scheme cxr)) (cadddr '(1 2 3 4 5))",new ScmInteger(4));
	}
}
