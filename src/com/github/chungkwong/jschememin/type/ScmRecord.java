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
public class ScmRecord extends ScmObject{
	private final ScmRecordType type;
	private final ScmObject[] fields;
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
	public ScmRecordType getType(){
		return type;
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append('(').append(type.getName().getValue()).append(' ');
		for(Map.Entry<ScmSymbol,Integer> entry:type.indices.entrySet())
			buf.append('(').append(entry.getKey().getValue()).append(' ').append(fields[entry.getValue()]).append(')');
		buf.append(')');
		return buf.toString();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}

}
