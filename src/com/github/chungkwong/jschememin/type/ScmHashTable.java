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
import com.github.chungkwong.jschememin.*;
import java.util.*;
import java.util.stream.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmHashTable extends ScmObject{
	private final HashMap<ObjectWithHash,ScmObject> table;
	private final Evaluable hash,equiv;
	private final ObjectWithHash keyWithHash=new ObjectWithHash(null);
	private final boolean mutable;
	private ScmHashTable(HashMap<ObjectWithHash,ScmObject> table,Evaluable hash,Evaluable equiv,boolean mutable){
		this.table=table;
		this.hash=hash;
		this.equiv=equiv;
		this.mutable=mutable;
	}
	public ScmHashTable(Evaluable hash,Evaluable equiv){
		this(new HashMap<>(),hash,equiv,true);
	}
	public ScmHashTable(Evaluable hash,Evaluable equiv,int k){
		this(new HashMap<>(k),hash,equiv,true);
	}
	public int size(){
		return table.size();
	}
	public ScmObject get(ScmObject key,ScmObject def){
		keyWithHash.setObject(key);
		ScmObject value=table.get(keyWithHash);
		return value==null?def:value;
	}
	public boolean contains(ScmObject key){
		keyWithHash.setObject(key);
		return table.containsKey(keyWithHash);
	}
	public void remove(ScmObject key){
		keyWithHash.setObject(key);
		table.remove(keyWithHash);
	}
	public void put(ScmObject key,ScmObject value){
		table.put(new ObjectWithHash(key),value);
	}
	public void clear(){
		table.clear();
	}
	public ScmVector keys(){
		ArrayList<ScmObject> keys=new ArrayList<>(table.size());
		table.forEach((k,v)->keys.add(k.getObject()));
		return new ScmVector(keys);
	}
	public ScmVector values(){
		ArrayList<ScmObject> values=new ArrayList<>(table.size());
		table.forEach((k,v)->values.add(v));
		return new ScmVector(values);
	}
	public ScmHashTable copy(boolean mutable){
		return new ScmHashTable(new HashMap<>(table),hash,equiv,mutable);
	}
	public Evaluable getEquivalenceFunction(){
		return equiv;
	}
	public Evaluable getHashFunction(){
		return hash;
	}
	public boolean isMutable(){
		return mutable;
	}
	@Override
	public String toExternalRepresentation(){
		return table.entrySet().stream().map((entry)->"("+entry.getKey()+" . "+entry.getValue()+")")
			.collect(Collectors.joining(" ","(",")"));
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	private class ObjectWithHash{
		private ScmObject object;
		public ObjectWithHash(ScmObject object){
			this.object=object;
		}
		public ScmObject getObject(){
			return object;
		}
		public void setObject(ScmObject object){
			this.object=object;
		}
		@Override
		public int hashCode(){
			return ((ScmComplex)hash.call(new ScmPair(object,ScmNil.NIL))).toScmInteger().getValue().intValue();
		}
		@Override
		public boolean equals(Object obj){
			return obj instanceof ObjectWithHash&&
					((ScmBoolean)equiv.call((ScmPairOrNil)ScmList.toList(object,((ObjectWithHash)obj).object))).isTrue();
		}
		@Override
		public String toString(){
			return object.toString();
		}
	}
}
