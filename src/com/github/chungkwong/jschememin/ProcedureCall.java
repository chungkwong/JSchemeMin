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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ProcedureCall implements Evaluable{
	public static final ProcedureCall CALL=new ProcedureCall();
	private static final ScmSymbol IF=new ScmSymbol("if");
	private static final ScmSymbol SET=new ScmSymbol("set!");
	private static final ScmSymbol LAMBDA=new ScmSymbol("lambda");
	private static final ScmSymbol QUOTE=new ScmSymbol("quote");
	private static final ScmSymbol INCLUDE=new ScmSymbol("include");
	private static final ScmSymbol INCLUDE_CI=new ScmSymbol("include-ci");
	private ProcedureCall(){
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject expr){
		if(pointer==null){
			startEvaluate(expr,cont,env);
		}else if(pointer instanceof ScmPair){
			if(expr==ScmBoolean.FALSE){
				cont.callTail(this,((ScmPair)((ScmPair)pointer).getCdr()).getCar());
			}else{
				cont.callTail(this,((ScmPair)pointer).getCar());
			}
		}else if(pointer instanceof ScmSymbol){
			env.set((ScmSymbol)pointer,expr);
			cont.ret(expr);
		}else{
			assert pointer instanceof BackTrace;
			BackTrace b=(BackTrace)pointer;
			if(expr instanceof ScmSyntaxRule){
				cont.callTail(this,((ScmSyntaxRule)expr).tranform(((BackTrace)pointer).getAfter()));
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
	}
	private void startEvaluate(ScmObject expr,Continuation cont,Environment env){
		if(expr.isSelfevaluating()){
			cont.ret(expr);
		}else if(expr instanceof ScmPair){
			ScmPair list=(ScmPair)expr;
			ScmObject car=list.getCar();
			if(car.equals(IF)){
				ScmPair remain=(ScmPair)list.getCdr();
				cont.call(this,(ScmPair)remain.getCdr(),remain.getCar());
			}else if(car.equals(SET)){
				ScmPair remain=(ScmPair)list.getCdr();
				cont.call(this,(ScmSymbol)remain.getCar(),((ScmPair)remain.getCdr()).getCar());
			}else if(car.equals(LAMBDA)){
				ScmPair remain=(ScmPair)list.getCdr();
				cont.ret(new ScmProcedure(remain.getCar(),(ScmPairOrNil)remain.getCdr(),env));
			}else if(car.equals(QUOTE)){
				cont.ret(list.getCdr());
			}else if(car.equals(INCLUDE)){
				cont.callTail(this,getFileContent(((ScmString)list.getCdr()).getValue(),true));
			}else if(car.equals(INCLUDE_CI)){
				cont.callTail(this,getFileContent(((ScmString)list.getCdr()).getValue(),true));
			}else
				cont.call(this,new BackTrace(null,null,(ScmPairOrNil)list.getCdr()),list.getCar());
		}else if(expr instanceof ScmSymbol){
			ScmObject val=env.get((ScmSymbol)expr);
			if(val!=null)
				cont.ret(val);
			else
				throw new UnsupportedOperationException("Undeclarated variable "+expr);
		}else{
			throw new UnsupportedOperationException("Not supported yet.");
		}
	}
	class BackTrace{
		private final ScmPair before, beforeLast;
		private final ScmPairOrNil after;
		public BackTrace(ScmPair before,ScmPair beforeLast,ScmPairOrNil after){
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
		public ScmPairOrNil getAfter(){
			return after;
		}
	}
	private ScmPair getFileContent(String file,boolean foldingCase){
		try{
			Parser parser=new Parser(new Lex(new InputStreamReader(new FileInputStream(file),"UTF-8"),foldingCase));
			return new ScmPair(new ScmSymbol("begin"),ScmPair.toList(parser.getRemainingDatums()));
		}catch(FileNotFoundException|UnsupportedEncodingException ex){
			throw new RuntimeException();
		}
	}
}
