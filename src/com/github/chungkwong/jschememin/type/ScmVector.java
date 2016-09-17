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
	public ScmVector set(int index,ScmObject element){
		vector.set(index,element);
		return this;
	}
	public Stream<ScmObject> stream(){
		return vector.stream();
	}

	/**
	 *
	 * @param obj
	 * @return
	 */
	@Override
	public boolean equals(Object obj){
		return obj==this||ObjectPair.equals(this,obj,new HashSet<>());
	}
	@Override
	public boolean equalsValue(ScmObject obj){
		return this==obj;
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
	public static ScmVector toVector(ScmPairOrNil list){
		int len=ScmList.getLength(list);
		ArrayList<ScmObject> vec=new ArrayList<>(len);
		ScmList.forEach(list,(item)->vec.add(item));
		return new ScmVector(vec);
	}
	public static ScmVector append(ScmPairOrNil list){
		return new ScmVector(ScmList.asStream(list).map((str)->((ScmVector)str).vector)
				.collect(ArrayList<ScmObject>::new,ArrayList<ScmObject>::addAll,ArrayList<ScmObject>::addAll));
	}
	public ScmPairOrNil toList(int start,int end){
		ScmListBuilder buf=new ScmListBuilder();
		for(int i=start;i<end;i++)
			buf.add(vector.get(i));
		return (ScmPairOrNil)buf.toList();
	}
	public static ScmVector fill(ScmObject o,int count){
		return new ScmVector(new ArrayList(Collections.nCopies(count,o)));
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}
	public ScmVector setRange(ScmObject item,int start,int end){
		for(int i=start;i<end;i++)
			vector.set(i,item);
		return this;
	}
	public ScmVector copyTo(ScmVector to,int at,int start,int end){
		while(start<end)
			to.vector.set(at++,vector.get(start++));
		return to;
	}
	public ScmVector copy(int start,int end){
		return new ScmVector(new ArrayList<>(vector.subList(start,end)));
	}
	public ScmString toScmString(int start,int end){
		return new ScmString(vector.subList(start,end).stream().mapToInt((c)->((ScmCharacter)c).getCodePoint()).
				collect(StringBuilder::new,StringBuilder::appendCodePoint,StringBuilder::append).toString());
	}
}