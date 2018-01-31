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
 * Correspoding to the primitive lambda in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Lambda extends BasicConstruct implements Primitive{
	public static final Lambda INSTANCE=new Lambda();
	private Lambda(){
		super(new ScmSymbol("lambda"));
	}
	@Override
	public void call(SchemeEnvironment env,Continuation cont,Object pointer,ScmPairOrNil expr){
		ScmPair list=(ScmPair)expr;
		cont.ret(new ScmProcedure(list.getCar(),(ScmPair)list.getCdr(),env));
	}
}