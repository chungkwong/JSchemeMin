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
public class Apply extends BasicConstruct{
	public static final Apply INSTANCE=new Apply();
	private Apply(){
		super(new ScmSymbol("apply"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil param){
		ScmListBuilder buf=new ScmListBuilder();
		Evaluable proc=(Evaluable)ScmList.first(param);
		ScmPair curr=(ScmPair)((ScmPair)param).getCdr();
		for(;curr.getCdr() instanceof ScmPair;curr=(ScmPair)curr.getCdr()){
			buf.add(curr.getCar());
		}
		ScmList.forEach(curr.getCar(),(o)->buf.add(o));
		cont.callTail(proc,buf.toList(),env);
	}
}