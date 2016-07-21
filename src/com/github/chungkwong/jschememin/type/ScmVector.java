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
import java.util.stream.*;
public final class ScmVector extends ScmObject{
	private final List<ScmObject> vector;
	private ScmVector(List<ScmObject> vector){
		this.vector=vector;
	}
	public ScmVector(ArrayList<ScmObject> vector){
		this.vector=vector;
	}
	public int getLength(){
		return vector.size();
	}
	public ScmObject get(int index){
		return vector.get(index);
	}
	public void set(int index,ScmObject element){
		vector.set(index,element);
	}
	public Stream<ScmObject> stream(){
		return vector.stream();
	}
	@Override
	public boolean equals(Object obj){
		return obj==this||ObjectPair.equals(this,obj,new HashSet<ObjectPair>());
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=97*hash+Objects.hashCode(this.vector);
		return hash;
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		IdentityHashMap<ScmObject,DatumRecord> refs=new IdentityHashMap<>();
		DatumRecord.collectReference(this,refs);
		toExternalRepresentation(buf,refs);
		return buf.toString();
	}
	boolean toExternalRepresentation(StringBuilder buf,IdentityHashMap<ScmObject,DatumRecord> refs){
		DatumRecord record=refs.get(this);
		if(record!=null&&record.isReused()){
			if(record.isDefined()){
				buf.append('#').append(record.getId()).append('#');
				return false;
			}else{
				buf.append('#').append(record.getId()).append('=');
				record.define();
			}
		}
		buf.append("#(");
		for(ScmObject obj:vector){
			DatumRecord.toExternalRepresentation(obj,buf,refs);
			buf.append(' ');
		}
		buf.append(')');
		return true;
	}
	public static ScmVector toVector(ScmObject... obj){
		return new ScmVector(Arrays.asList(obj));
	}
	public static ScmVector fillVector(int count,ScmObject o){
		return new ScmVector(Collections.nCopies(count,o));
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}
}