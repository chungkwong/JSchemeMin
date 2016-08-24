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
public class ExpressionEvaluator extends Evaluable implements Primitive{
	public static final ExpressionEvaluator INSTANCE=new ExpressionEvaluator();
	private ExpressionEvaluator(){
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil expr){
		if(pointer==null){
			evaluateFirst(ScmList.first(expr),cont,env);
		}else{
			evaluateRemaining((BackTrace)pointer,ScmList.first(expr),env,cont);
		}
	}
	private void evaluateFirst(ScmObject expr,Continuation cont,Environment env){
		if(expr.isSelfevaluating()){
			cont.ret(expr);
		}else if(expr instanceof ScmPair){
			ScmPair list=(ScmPair)expr;
			cont.call(this,new BackTrace(null,null,list.getCdr()),list.getCar(),env);
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
		if(b.getBefore()==null){
			if(expr instanceof Primitive){
				((Evaluable)expr).call(env,cont,null,(ScmPairOrNil)b.getAfter());
			}else if(expr instanceof ScmSyntaxRules){
				cont.callTail(this,((ScmSyntaxRules)expr).transform((ScmPairOrNil)b.getAfter(),env),env);
			}else if(b.getAfter()==ScmNil.NIL){
				if(!(expr instanceof Evaluable))
					throw new RuntimeException("Expect Evaluable:"+expr);
				cont.callTail((Evaluable)expr,ScmNil.NIL,env);
			}else{
				ScmPair before=new ScmPair(expr,ScmNil.NIL);
				cont.call(this,new BackTrace(before,before,(ScmPairOrNil)((ScmPair)b.getAfter()).getCdr()),((ScmPair)b.getAfter()).getCar(),env);
			}
		}else{
			ScmPair newBeforeLast=new ScmPair(expr,ScmNil.NIL);
			b.getBeforeLast().setCdr(newBeforeLast);
			if(b.getAfter()==ScmNil.NIL){
				if(!(b.getBefore().getCar() instanceof Evaluable))
					throw new RuntimeException("Expect Evaluable:"+b.getBefore().getCar());
				cont.callTail((Evaluable)b.getBefore().getCar(),(ScmPairOrNil)b.getBefore().getCdr(),env);
			}else{
				cont.call(this,new BackTrace(b.getBefore(),newBeforeLast,(ScmPairOrNil)((ScmPair)b.getAfter()).getCdr()),((ScmPair)b.getAfter()).getCar(),env);
			}
		}
	}
	@Override
	public String toExternalRepresentation(){
		return "apply";
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
