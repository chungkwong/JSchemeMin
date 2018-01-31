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
 * A helper that may construct list by reverse order in the structure
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmListBuilder{
	private ScmObject start=ScmNil.NIL;
	private ScmPair end=null;
	/**
	 * Prepare for a list
	 */
	public ScmListBuilder(){

	}
	/**
	 * Provides a item to be added
	 * @param item
	 */
	public void add(ScmObject item){
		ScmPair fresh=new ScmPair(item,ScmNil.NIL);
		if(end==null){
			start=end=fresh;
		}else{
			end.setCdr(fresh);
			end=fresh;
		}
	}
	/**
	 * Provides some items to be added
	 * @param list
	 */
	public void addAll(ScmPairOrNil list){
		ScmList.forEach(list,(item)->add(item));
	}
	/**
	 * Provides some items to be added
	 * @param buf
	 */
	public void addAll(ScmListBuilder buf){
		if(end==null){
			start=buf.start;
			end=buf.end;
		}else if(buf.end!=null){
			end.setCdr(buf.start);
			end=buf.end;
		}
	}
	/**
	 * Set the last item in a partial list
	 * @param obj
	 */
	public void setLast(ScmObject obj){
		if(end==null)
			start=obj;
		else
			end.setCdr(obj);
	}
	/**
	 * Build the list
	 * @return the list
	 */
	public ScmObject toList(){
		return start;
	}
}
