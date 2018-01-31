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
import com.github.chungkwong.jschememin.lib.*;
import java.util.*;
/**
 * Represents the type of records in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmRecordType extends ScmObject{
	private final ScmSymbol name;
	final HashMap<ScmSymbol,Integer> indices;
	/**
	 * Create a record type
	 * @param name the name of the type
	 * @param fields the fields which the records have
	 */
	public ScmRecordType(ScmSymbol name,ScmObject fields){
		this.name=name;
		this.indices=new HashMap<>();
		int i=0;
		while(fields instanceof ScmPair){
			indices.put((ScmSymbol)((ScmPair)fields).getCar(),i++);
			fields=((ScmPair)fields).getCdr();
		}
	}
	/**
	 * Get the name of the type
	 * @return the name
	 */
	public ScmSymbol getName(){
		return name;
	}
	@Override
	public String toExternalRepresentation(){
		return name.getValue();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmRecordType&&((ScmRecordType)obj).name.equals(name);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=73*hash+Objects.hashCode(this.name);
		return hash;
	}
	/**
	 * Get the constructor of the type
	 * @return the procedure
	 */
	public NativeEvaluable getConstractor(){
		return new NativeEvaluable(new Constructor());
	}
	/**
	 * Get the predicate of the type
	 * @return the procedure
	 */
	public NativeEvaluable getPredicate(){
		return new NativeEvaluable(new Predicate());
	}
	/**
	 * Get the accessor of a field of the type
	 * @param field the field
	 * @return the procedure
	 */
	public NativeEvaluable getAccessor(ScmSymbol field){
		return new NativeEvaluable(new Accessor(field));
	}
	/**
	 * Get the modifier of a field of the type
	 * @param field the field
	 * @return the procedure
	 */
	public NativeEvaluable getModifier(ScmSymbol field){
		return new NativeEvaluable(new Modifier(field));
	}
	class Constructor implements NativeProcedure{
		@Override
		public ScmObject call(ScmObject fields){
			ScmObject[] data=new ScmObject[indices.size()];
			int i=0;
			while(fields instanceof ScmPair){
				data[i++]=((ScmPair)fields).getCar();
				fields=((ScmPair)fields).getCdr();
			}
			return new ScmRecord(ScmRecordType.this,data);
		}
	}
	class Predicate implements NativeProcedure{
		private Predicate(){

		}
		@Override
		public ScmObject call(ScmObject param){
			ScmObject obj=ScmList.first(param);
			return ScmBoolean.valueOf(obj instanceof ScmRecord&&((ScmRecord)obj).getType()==ScmRecordType.this);
		}
	}
	class Accessor implements NativeProcedure{
		final int index;
		private Accessor(ScmSymbol field){
			index=indices.get(field);
		}
		@Override
		public ScmObject call(ScmObject param){
			return ((ScmRecord)ScmList.first(param)).get(index);
		}
	}
	class Modifier implements NativeProcedure{
		final int index;
		private Modifier(ScmSymbol field){
			index=indices.get(field);
		}
		@Override
		public ScmObject call(ScmObject param){
			ScmRecord record=(ScmRecord)ScmList.first(param);
			ScmObject obj=((ScmPair)((ScmPair)param).getCdr()).getCar();
			record.set(index,obj);
			return record;
		}
	}
}