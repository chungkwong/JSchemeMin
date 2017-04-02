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
	public void call(SchemeEnvironment env,Continuation cont,Object pointer,ScmPairOrNil param){
		if(pointer==null){
			Backtrack track=new Backtrack((Evaluable)ScmList.first(param),
					(Evaluable)ScmList.second(param),(Evaluable)ScmList.third(param),false);
			cont.call(track.getBefore(),track,ScmNil.NIL,env);
		}else if(pointer instanceof Backtrack){
			Backtrack old=(Backtrack)pointer;
			if(old.isStarted())
				cont.call(((Backtrack)pointer).getAfter(),param,ScmNil.NIL,env);
			else
				cont.call(((Backtrack)pointer).getThunk(),new Backtrack(old.getBefore(),old.getThunk(),old.getAfter(),true),ScmNil.NIL,env);
		}else{
			cont.ret((ScmPairOrNil)pointer);
		}
	}
	public static class Backtrack{
		private final Evaluable before;
		private final Evaluable thunk;
		private final Evaluable after;
		private final boolean started;
		public Backtrack(Evaluable before,Evaluable thunk,Evaluable after,boolean started){
			this.before=before;
			this.thunk=thunk;
			this.after=after;
			this.started=started;
		}
		public Evaluable getBefore(){
			return before;
		}
		public Evaluable getThunk(){
			return thunk;
		}
		public Evaluable getAfter(){
			return after;
		}
		public boolean isStarted(){
			return started;
		}
	}
}