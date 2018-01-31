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
 * Represents records in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmRecord extends ScmObject{
	private final ScmRecordType type;
	private final ScmObject[] fields;
	/**
	 * Create a record
	 * @param type the type of the record
	 * @param fields the fields that the record have
	 */
	public ScmRecord(ScmRecordType type,ScmObject[] fields){
		this.type=type;
		this.fields=fields;
	}
	ScmObject get(int i){
		return fields[i];
	}
	void set(int i,ScmObject obj){
		fields[i]=obj;
	}
	/**
	 * Get the type of the record
	 * @return the type
	 */
	public ScmRecordType getType(){
		return type;
	}
	@Override
	public String toExternalRepresentation(){
		ScmListBuilder buf=new ScmListBuilder();
		buf.add(type.getName());
		type.indices.forEach((key,value)->buf.add(new ScmPair(key,fields[value])));
		return buf.toList().toString();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmRecord&&((ScmRecord)obj).type.equals(type)
				&&Arrays.equals(((ScmRecord)obj).fields,fields);
	}
	@Override
	public boolean equalsValue(ScmObject obj){
		return this==obj;
	}
	@Override
	public int hashCode(){
		int hash=5;
		hash=83*hash+Objects.hashCode(this.type);
		hash=83*hash+Arrays.deepHashCode(this.fields);
		return hash;
	}
}
