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
public final class ScmPair extends ScmPairOrNil{
	private ScmObject car;
	private ScmObject cdr;
	public ScmPair(ScmObject car,ScmObject cdr){
		this.car=car;
		this.cdr=cdr;
	}
	public ScmObject getCar(){
		return car;
	}
	public ScmObject getCdr(){
		return cdr;
	}
	public ScmObject getCaar(){
		return ((ScmPair)car).getCar();
	}
	public ScmObject getCadr(){
		return ((ScmPair)cdr).getCar();
	}
	public ScmObject getCddr(){
		return ((ScmPair)cdr).getCdr();
	}
	public ScmObject getCaddr(){
		return ((ScmPair)((ScmPair)cdr).getCdr()).getCar();
	}
	public void setCar(ScmObject car){
		this.car=car;
	}
	public void setCdr(ScmObject cdr){
		this.cdr=cdr;
	}
	@Override
	public boolean equals(Object obj){
		return obj==this||ObjectPair.equals(this,obj,new HashSet<ObjectPair>());
	}
	@Override
	public boolean equalsValue(ScmObject obj){
		return this==obj;
	}
	@Override
	public int hashCode(){
		int hash=7;
		if(!(car instanceof ScmPair||car instanceof ScmVector))
			hash=13*hash+Objects.hashCode(this.car);
		if(!(cdr instanceof ScmPair||cdr instanceof ScmVector))
			hash=13*hash+Objects.hashCode(this.cdr);
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
		buf.append("(");
		DatumRecord.toExternalRepresentation(car,buf,refs);
		ScmObject next=cdr;
		boolean noCycle=true;
		while(next instanceof ScmPair&&!refs.get(next).isReused()){
			buf.append(" ");
			DatumRecord.toExternalRepresentation(((ScmPair)next).getCar(),buf,refs);
			next=((ScmPair)next).getCdr();
		}
		if(!(next instanceof ScmNil)){
			buf.append(" . ");
			DatumRecord.toExternalRepresentation(next,buf,refs);
		}
		buf.append(")");
		return true;
	}
}
