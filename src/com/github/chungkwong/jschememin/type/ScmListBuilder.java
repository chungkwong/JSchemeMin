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
package com.github.chungkwong.jschememin.type;

/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmListBuilder{
	private ScmPairOrNil start=ScmNil.NIL;
	private ScmPair end=null;
	public ScmListBuilder(){

	}
	public void add(ScmObject item){
		ScmPair fresh=new ScmPair(item,ScmNil.NIL);
		if(end==null){
			start=end=fresh;
		}else{
			end.setCdr(fresh);
			end=fresh;
		}
	}
	public void addAll(ScmPairOrNil list){
		ScmList.forEach(list,(item)->add(item));
	}
	public void setLast(ScmObject obj){
		end.setCdr(obj);
	}
	public ScmPairOrNil toList(){
		return start;
	}
}
