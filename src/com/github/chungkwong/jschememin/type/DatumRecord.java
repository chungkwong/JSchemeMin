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
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class DatumRecord{
	private static int idCounter=0;
	private final int id;
	private boolean reused,defined;
	public DatumRecord(){
		this.id=++idCounter;
		this.reused=false;
		this.defined=false;
	}
	public void reuse(){
		reused=true;
	}
	public boolean isReused(){
		return reused;
	}
	public void define(){
		defined=true;
	}
	public boolean isDefined(){
		return defined;
	}
	public int getId(){
		return id;
	}
	public static void updateId(int counter){
		if(counter>idCounter)
			idCounter=counter;
	}
	@Override
	public String toString(){
		return super.toString(); //To change body of generated methods, choose Tools | Templates.
	}
	public static void collectReference(ScmObject object,IdentityHashMap<ScmObject,DatumRecord> map){
		if(map.containsKey(object))
			map.get(object).reuse();
		else if(object instanceof ScmVector){
			map.put(object,new DatumRecord());
			for(int i=0;i<((ScmVector)object).getLength();i++)
				collectReference(((ScmVector)object).get(i),map);
		}else if(object instanceof ScmPair){
			map.put(object,new DatumRecord());
			collectReference(((ScmPair)object).getCar(),map);
			collectReference(((ScmPair)object).getCdr(),map);
		}
	}
	public static boolean  toExternalRepresentation(ScmObject obj,StringBuilder buf,IdentityHashMap<ScmObject,DatumRecord> refs){
		if(obj instanceof ScmVector){
			return ((ScmVector)obj).toExternalRepresentation(buf,refs);
		}else if(obj instanceof ScmPair){
			return ((ScmPair)obj).toExternalRepresentation(buf,refs);
		}else{
			buf.append(obj.toExternalRepresentation());
			return true;
		}
	}
}