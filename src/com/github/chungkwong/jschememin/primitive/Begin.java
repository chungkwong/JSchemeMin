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
public class Begin extends PrimitiveType{
	public static final Begin INSTANCE=new Begin();
	private Begin(){
		super(new ScmSymbol("begin"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject param){
		cont.replaceCurrent(this);
		if(pointer==null){
			call(env,cont,param,null);
		}else if(pointer instanceof ScmPair){
			ScmObject next=((ScmPair)pointer).getCdr();
			if(next==ScmNil.NIL){
				cont.callTail(ExpressionEvaluator.INSTANCE,((ScmPair)pointer).getCar());
			}else if(next instanceof ScmPair){
				cont.call(ExpressionEvaluator.INSTANCE,((ScmPair)next).getCdr(),((ScmPair)pointer).getCar());
			}else
				throw new SyntaxException();
		}else
			cont.ret(new ScmSymbol("unknown"));
	}

}
