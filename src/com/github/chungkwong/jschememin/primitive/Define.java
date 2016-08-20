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
public class Define extends BasicConstruct implements Primitive{
	public static final Define INSTANCE=new Define();
	private Define(){
		super(new ScmSymbol("define"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject expr){
		if(pointer==null){
			ScmPair remain=(ScmPair)expr;
			if(remain.getCar() instanceof ScmSymbol){
				cont.replaceCurrent(this);
				cont.call(ExpressionEvaluator.INSTANCE,(ScmSymbol)remain.getCar(),((ScmPair)remain.getCdr()).getCar(),env);
			}else if(remain.getCar() instanceof ScmPair){
				ScmSymbol name=(ScmSymbol)((ScmPair)remain.getCar()).getCar();
				ScmProcedure proc=new ScmProcedure(((ScmPair)remain.getCar()).getCdr(),(ScmPair)remain.getCdr(),env);
				env.add(name,proc);
				cont.ret(proc);
			}else
				throw new SyntaxException();
		}else{
			env.add((ScmSymbol)pointer,expr);
			cont.ret(expr);
		}
	}
}