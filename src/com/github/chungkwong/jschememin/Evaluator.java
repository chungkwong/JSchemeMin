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
import com.github.chungkwong.jschememin.primitive.*;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Evaluator{
	private final Environment env;
	private final HashSet<ScmPair> imported=new HashSet<>();
	private final Continuation cont=new Continuation();
	private static final ScmSymbol ok=new ScmSymbol("ok"),fail=new ScmSymbol("fail");
	public Evaluator(boolean repl){
		this(new Environment(repl));
	}
	public Evaluator(Environment env){
		this.env=env;
		env.addPrimitiveType(If.INSTANCE);
		env.addPrimitiveType(Assignment.INSTANCE);
		env.addPrimitiveType(Lambda.INSTANCE);
		env.addPrimitiveType(Include.INSTANCE);
		env.addPrimitiveType(Include.INSTANCE_CI);
		env.addPrimitiveType(Quote.INSTANCE);
		env.addPrimitiveType(Begin.INSTANCE);
		env.addPrimitiveType(Define.INSTANCE);
		env.addPrimitiveType(DefineSyntax.INSTANCE);
		env.addPrimitiveType(DefineRecordType.INSTANCE);
		env.addPrimitiveType(DefineLibrary.INSTANCE);
		env.addPrimitiveType(Import.INSTANCE);
	}
	public ScmObject eval(ScmObject expr){
		cont.callInit(ExpressionEvaluator.INSTANCE,expr);
		while(cont.hasNext())
			cont.evalNext(env);
		return cont.getValue();
	}
	public Environment getEnvironment(){
		return env;
	}
	public static void main(String[] args) throws Exception{
		Scanner in=new Scanner(System.in);
		Evaluator eval=new Evaluator(true);
		while(in.hasNextLine()){
			new Parser(in.nextLine()).getRemainingDatums().forEach((d)->System.out.println(eval.eval(d)));
		}
	}
}