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
public class DynamicWind extends BasicConstruct{
	public static final DynamicWind INSTANCE=new DynamicWind();
	private DynamicWind(){
		super(new ScmSymbol("dynamic-wind"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject param){
		if(pointer==null)
			call(env,cont,new Backtrack((ScmPairOrNil)param,(ScmPairOrNil)param,null),param);
		else if(((Backtrack)pointer).getThen() instanceof ScmPair){
			ScmPair then=(ScmPair)((Backtrack)pointer).getThen();
			ScmPair all=(ScmPair)((Backtrack)pointer).getAll();
			ScmObject ret=null;
			if(then.getCddr() instanceof ScmNil)
				ret=((ScmPair)param).getCar();
			cont.call(ExpressionEvaluator.INSTANCE,new Backtrack(all,(ScmPairOrNil)then.getCdr(),ret),ScmList.toList(then.getCar()),env);
		}else{
			cont.ret(((Backtrack)pointer).getRet());
		}
	}
	private static class Backtrack{
		private final ScmPairOrNil all;
		private final ScmPairOrNil then;
		private final ScmObject ret;
		public Backtrack(ScmPairOrNil all,ScmPairOrNil then,ScmObject ret){
			this.all=all;
			this.then=then;
			this.ret=ret;
		}
		public ScmPairOrNil getAll(){
			return all;
		}
		public ScmPairOrNil getThen(){
			return then;
		}
		public ScmObject getRet(){
			return ret;
		}
	}
}