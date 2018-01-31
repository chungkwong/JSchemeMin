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
import javax.script.*;
/**
 * Evaluator for Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Evaluator extends AbstractScriptEngine{
	private final SchemeEnvironment env;
	/**
	 * Create a evaluator
	 * @param repl REPL mode or not
	 */
	public Evaluator(boolean repl){
		this(new SchemeEnvironment(repl));
	}
	/**
	 * Create a evaluator
	 * @param env top-level environemnt
	 */
	public Evaluator(SchemeEnvironment env){
		this.env=env;
	}
	/**
	 * Evaluate a expression
	 * @param expr the expression
	 * @return the value
	 */
	public ScmObject eval(ScmObject expr){
		Continuation cont=new Continuation();
		cont.callInit(ExpressionEvaluator.INSTANCE,expr,env);
		while(cont.hasNext())
			cont.evalNext();
		return cont.getCurrentValue();
	}
	/**
	 * Get the top level environment
	 * @return
	 */
	public SchemeEnvironment getEnvironment(){
		return env;
	}
	@Override
	public Object eval(String script,ScriptContext context) throws ScriptException{
		return eval(new StringReader(script),context);
	}
	@Override
	public Object eval(Reader reader,ScriptContext context) throws ScriptException{
		Parser parser=new Parser(new Lex(reader));
		env.setParent(new JavaEnvironment(context,env.isREPL()));
		ScmObject datum,ret=null;
		while((datum=parser.nextDatum())!=null){
			ret=eval(datum);
		}
		return ret;
	}
	@Override
	public Bindings createBindings(){
		Bindings bindings=new SimpleBindings();
		return bindings;
	}
	@Override
	public ScriptEngineFactory getFactory(){
		return EvaluatorFactory.INSTANCE;
	}
}