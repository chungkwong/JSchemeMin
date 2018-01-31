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
 * Represents the type hashtable in Scheme
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
	/**
	 * Construct a hashtable
	 * @param hash the hash function
	 * @param equiv the equal function
	 */
	public ScmHashTable(Evaluable hash,Evaluable equiv){
		this(new HashMap<>(),hash,equiv,true);
	}
	/**
	 * Construct a hashtable
	 * @param hash the hash function
	 * @param equiv the equal function
	 * @param k the expected size
	 */
	public ScmHashTable(Evaluable hash,Evaluable equiv,int k){
		this(new HashMap<>(k),hash,equiv,true);
	}
	/**
	 * Corresponding the procedure hashtable-size in Scheme
	 * @return
	 */
	public int size(){
		return table.size();
	}
	/**
	 * Corresponding the procedure hashtable-ref in Scheme
	 * @param key
	 * @param def
	 * @return
	 */
	public ScmObject get(ScmObject key,ScmObject def){
		keyWithHash.setObject(key);
		ScmObject value=table.get(keyWithHash);
		return value==null?def:value;
	}
	/**
	 * Corresponding the procedure hashtable-contains in Scheme
	 * @param key
	 * @return
	 */
	public boolean contains(ScmObject key){
		keyWithHash.setObject(key);
		return table.containsKey(keyWithHash);
	}

	/**
	 * Corresponding the procedure hashtable-delete in Scheme
	 * @param key
	 */
	public void remove(ScmObject key){
		keyWithHash.setObject(key);
		table.remove(keyWithHash);
	}

	/**
	 * Corresponding the procedure hashtable-set! in Scheme
	 * @param key
	 * @param value
	 */
	public void put(ScmObject key,ScmObject value){
		table.put(new ObjectWithHash(key),value);
	}

	/**
	 * Corresponding the procedure hashtable-clear! in Scheme
	 */
	public void clear(){
		table.clear();
	}

	/**
	 * Corresponding the procedure hashtable-keys in Scheme
	 * @return
	 */
	public ScmVector keys(){
		ArrayList<ScmObject> keys=new ArrayList<>(table.size());
		table.forEach((k,v)->keys.add(k.getObject()));
		return new ScmVector(keys);
	}

	/**
	 * Get the vector of values
	 * @return
	 */
	public ScmVector values(){
		ArrayList<ScmObject> values=new ArrayList<>(table.size());
		table.forEach((k,v)->values.add(v));
		return new ScmVector(values);
	}
	/**
	 * Corresponding the procedure hashtable-copy in Scheme
	 * @param mutable
	 * @return
	 */
	public ScmHashTable copy(boolean mutable){
		return new ScmHashTable(new HashMap<>(table),hash,equiv,mutable);
	}

	/**
	 * Corresponding the procedure hashtable-equivalence-function in Scheme
	 * @return
	 */
	public Evaluable getEquivalenceFunction(){
		return equiv;
	}
	/**
	 * Corresponding the procedure hashtable-hash-function in Scheme
	 * @return
	 */
	public Evaluable getHashFunction(){
		return hash;
	}
	/**
	 * Corresponding the procedure hashtable-mutable? in Scheme
	 * @return
	 */
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
			return ((ScmComplex)hash.call(ScmList.toList(object))).toScmInteger().getValue().intValue();
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
