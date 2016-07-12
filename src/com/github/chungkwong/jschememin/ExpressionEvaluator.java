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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ExpressionEvaluator implements Evaluable{
	public static final ExpressionEvaluator INSTANCE=new ExpressionEvaluator();
	private ExpressionEvaluator(){
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject expr){
		if(pointer==null){
			evaluateFirst(expr,cont,env);
		}else{
			evaluateRemaining((BackTrace)pointer,expr,env,cont);
		}
	}
	private void evaluateFirst(ScmObject expr,Continuation cont,Environment env){
		if(expr.isSelfevaluating()){
			cont.ret(expr);
		}else if(expr instanceof ScmPair){
			ScmPair list=(ScmPair)expr;
			cont.call(this,new BackTrace(null,null,list.getCdr()),list.getCar());
		}else if(expr instanceof ScmSymbol){
			ScmObject val=env.get((ScmSymbol)expr);
			if(val!=null)
				cont.ret(val);
			else
				throw new UnsupportedOperationException("Undeclarated variable "+expr);
		}else{
			throw new SyntaxException();
		}
	}
	private void evaluateRemaining(BackTrace b,ScmObject expr,Environment env,Continuation cont){
		if(expr instanceof PrimitiveType){
			((PrimitiveType)expr).call(env,cont,null,b.getAfter());
		}else if(expr instanceof ScmSyntaxRules){
			cont.callTail(this,((ScmSyntaxRules)expr).tranform((ScmPairOrNil)b.getAfter()));
		}else if(b.getBefore()==null){
			if(b.getAfter()==ScmNil.NIL){
				cont.callTail((Evaluable)expr,ScmNil.NIL);
			}else{
				ScmPair before=new ScmPair(expr,ScmNil.NIL);
				cont.call(this,new BackTrace(before,before,(ScmPairOrNil)((ScmPair)b.getAfter()).getCdr()),((ScmPair)b.getAfter()).getCar());
			}
		}else{
			ScmPair newBeforeLast=new ScmPair(expr,ScmNil.NIL);
			b.getBeforeLast().setCdr(newBeforeLast);
			if(b.getAfter()==ScmNil.NIL){
				cont.callTail((Evaluable)b.getBefore().getCar(),b.getBefore().getCdr());
			}else{
				cont.call(this,new BackTrace(b.getBefore(),newBeforeLast,(ScmPairOrNil)((ScmPair)b.getAfter()).getCdr()),((ScmPair)b.getAfter()).getCar());
			}
		}
	}
	class BackTrace{
		private final ScmPair before, beforeLast;
		private final ScmObject after;
		public BackTrace(ScmPair before,ScmPair beforeLast,ScmObject after){
			this.before=before;
			this.beforeLast=beforeLast;
			this.after=after;
		}
		public ScmPair getBefore(){
			return before;
		}
		public ScmPair getBeforeLast(){
			return beforeLast;
		}
		public ScmObject getAfter(){
			return after;
		}
	}
}
