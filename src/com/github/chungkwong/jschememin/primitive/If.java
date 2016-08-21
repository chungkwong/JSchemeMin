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
public class If extends BasicConstruct implements Primitive{
	public static final If INSTANCE=new If();
	private If(){
		super(new ScmSymbol("if"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil expr){
		if(pointer==null){
			ScmPair list=(ScmPair)expr;
			cont.replaceCurrent(this);
			cont.call(ExpressionEvaluator.INSTANCE,(ScmPair)list.getCdr(),list.getCar(),env);
		}else{
			ScmPair list=(ScmPair)pointer;
			if(ScmList.first(expr)==ScmBoolean.FALSE){
				if(list.getCdr() instanceof ScmPair)
					cont.callTail(ExpressionEvaluator.INSTANCE,list.getCadr(),env);
				else
					cont.ret(new ScmSymbol("unspecified"));
			}else{
				cont.callTail(ExpressionEvaluator.INSTANCE,list.getCar(),env);
			}
		}
	}

}
