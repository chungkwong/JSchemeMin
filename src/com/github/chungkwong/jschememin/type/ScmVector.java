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
/**
 * Representation of the type vector in Scheme
 * @author kwong
 */
public final class ScmVector extends ScmObject{
	private final List<ScmObject> vector;
	private ScmVector(List<ScmObject> vector){
		this.vector=vector;
	}
	/**
	 * Construct a vector
	 * @param vector the list which the vector backed by
	 */
	public ScmVector(ArrayList<ScmObject> vector){
		this.vector=vector;
	}
	/**
	 * Get the length of the vector
	 * @return the length of the vector
	 */
	public int getLength(){
		return vector.size();
	}
	/**
	 * Get a elements in a vector
	 * @param index the index of that element
	 * @return the element
	 */
	public ScmObject get(int index){
		return vector.get(index);
	}
	/**
	 * Set a element in the vector
	 * @param index the index of that element
	 * @param element the new value
	 * @return this
	 */
	public ScmVector set(int index,ScmObject element){
		vector.set(index,element);
		return this;
	}
	public Stream<ScmObject> stream(){
		return vector.stream();
	}
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
	/**
	 * Build a vector
	 * @param obj the elements of the vector
	 * @return the new vector
	 */
	public static ScmVector toVector(ScmObject... obj){
		return new ScmVector(Arrays.asList(obj));
	}
	/**
	 * Build a vector
	 * @param list the elements of the vector
	 * @return the new vector
	 */
	public static ScmVector toVector(ScmPairOrNil list){
		int len=ScmList.getLength(list);
		ArrayList<ScmObject> vec=new ArrayList<>(len);
		ScmList.forEach(list,(item)->vec.add(item));
		return new ScmVector(vec);
	}
	/**
	 * Build a vector containing elements of this vector and the others
	 * @param list the others elements
	 * @return the new vector
	 */
	public static ScmVector append(ScmPairOrNil list){
		return new ScmVector(ScmList.asStream(list).map((str)->((ScmVector)str).vector)
				.collect(ArrayList<ScmObject>::new,ArrayList<ScmObject>::addAll,ArrayList<ScmObject>::addAll));
	}
	/**
	 * Build a list containing the elements of this vector
	 * @param start index of the first element
	 * @param end index after the last element
	 * @return the list
	 */
	public ScmPairOrNil toList(int start,int end){
		return (ScmPairOrNil)vector.subList(start,end).stream().collect(ScmList.COLLECTOR);
	}
	/**
	 * Build a vector which all elements are the same
	 * @param o the element
	 * @param count the length of the vector
	 * @return the new vector
	 */
	public static ScmVector fill(ScmObject o,int count){
		return new ScmVector(new ArrayList(Collections.nCopies(count,o)));
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}
	/**
	 * Set some elements to be the same
	 * @param item the new element
	 * @param start index of the first element
	 * @param end index after the last element
	 * @return this
	 */
	public ScmVector setRange(ScmObject item,int start,int end){
		for(int i=start;i<end;i++)
			vector.set(i,item);
		return this;
	}
	/**
	 * Copy some elements from this vector to override elements in another
	 * @param to the other vector
	 * @param at index of the first element to be copied
	 * @param start index of the first element
	 * @param end index after the last element
	 * @return
	 */
	public ScmVector copyTo(ScmVector to,int at,int start,int end){
		while(start<end)
			to.vector.set(at++,vector.get(start++));
		return to;
	}
	/**
	 * Copy some of the elements to a new vector
	 * @param start index of the first element
	 * @param end index after the last element
	 * @return the new vector
	 */
	public ScmVector copy(int start,int end){
		return new ScmVector(new ArrayList<>(vector.subList(start,end)));
	}
	/**
	 * Get the String containing the codepoints in this vector
	 * @param start index of the first element
	 * @param end index after the last element
	 * @return the String
	 */
	public ScmString toScmString(int start,int end){
		return new ScmString(vector.subList(start,end).stream().mapToInt((c)->((ScmCharacter)c).getCodePoint()).
				collect(StringBuilder::new,StringBuilder::appendCodePoint,StringBuilder::append).toString());
	}
}