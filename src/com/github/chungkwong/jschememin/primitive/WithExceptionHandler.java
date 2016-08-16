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
package com.github.chungkwong.jschememin.primitive;
import com.github.chungkwong.jschememin.*;
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class WithExceptionHandler extends PrimitiveType{
	public static final WithExceptionHandler INSTANCE=new WithExceptionHandler();
	private WithExceptionHandler(){
		super(new ScmSymbol("with-exception-handler"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject param){
		if(pointer==null){
			cont.replaceCurrent(this);
			cont.call(ExpressionEvaluator.INSTANCE,((ScmPair)param).getCar(),((ScmPair)param).getCdr(),env);
		}else if(pointer instanceof ErrorInfo)
			cont.callTail(ExpressionEvaluator.INSTANCE,ScmList.toList(
					((ErrorInfo)pointer).getHandler(),
					ScmList.toList(new ScmSymbol("quote"),((ScmPair)param).getCar())),env);
		else
			cont.ret(((ScmPair)param).getCar());
	}
	public static class ErrorInfo{
		private final ScmObject handler;
		public ErrorInfo(ScmObject handler){
			this.handler=handler;
		}
		public ScmObject getHandler(){
			return handler;
		}
	}
}