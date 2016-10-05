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
public class CallWithCurrentContinuation extends BasicConstruct{
	public static final CallWithCurrentContinuation INSTANCE=new CallWithCurrentContinuation();
	private CallWithCurrentContinuation(){
		super(new ScmSymbol("call-with-current-continuation"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil param){
		cont.callTail((Evaluable)ScmList.first(param),ScmList.toList(getRollbackProcedure(cont.getCopy())),env);
	}
	static final ScmObject getRollbackProcedure(Continuation checkpoint){
		return new Evaluable(){
			@Override
			public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil param){
				cont.reset(checkpoint);
				cont.ret((ScmPairOrNil)param);
			}
			@Override
			public String toExternalRepresentation(){
				return "roll-back-procedure";
			}
		};
	}
}