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
 * Represents the Scheme's type that wrap Java's type
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmJavaObject extends ScmObject{
	private final Object obj;
	/**
	 * Wrap a Java's Object
	 * @param obj
	 */
	public ScmJavaObject(Object obj){
		this.obj=obj;
	}
	/**
	 * Get the Java's object
	 * @return
	 */
	public Object getJavaObject(){
		return obj;
	}
	@Override
	public boolean equals(Object o){
		return o instanceof ScmJavaObject&&Objects.equals(((ScmJavaObject)o).obj,obj);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=53*hash+Objects.hashCode(this.obj);
		return hash;
	}
	@Override
	public String toExternalRepresentation(){
		return ScmList.toList(new ScmSymbol("java"),new ScmString(Objects.toString(obj))).toExternalRepresentation();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
}