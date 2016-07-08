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
import java.util.function.*;
public final class ScmPair<A extends ScmObject,D extends ScmObject> extends ScmPairOrNil{
	private A car;
	private D cdr;
	public ScmPair(A car,D cdr){
		this.car=car;
		this.cdr=cdr;
	}
	public A getCar(){
		return car;
	}
	public D getCdr(){
		return cdr;
	}
	public ScmObject getCadr(){
		return ((ScmPair)cdr).getCar();
	}
	public ScmObject getCddr(){
		return ((ScmPair)cdr).getCdr();
	}
	public void setCar(A car){
		this.car=car;
	}
	public void setCdr(D cdr){
		this.cdr=cdr;
	}
	public void forEach(Consumer<ScmObject> proc){
		ScmObject pair=this;
		while(pair instanceof ScmPair){
			proc.accept(((ScmPair)pair).getCar());
			pair=((ScmPair)pair).getCdr();
		}
	}
	@Override
	public boolean equals(Object obj){
		return obj==this||ObjectPair.equals(this,obj,new HashSet<ObjectPair>());
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
	public static ScmPairOrNil toList(List<? extends ScmObject> list){
		if(list.isEmpty())
			return ScmNil.NIL;
		ScmPair start=new ScmPair(list.get(0),ScmNil.NIL),end=start;
		for(ScmObject o:list.subList(1,list.size())){
			ScmPair newend=new ScmPair(o,ScmNil.NIL);
			end.setCdr(newend);
			end=newend;
		}
		return start;
	}
	public static ScmPairOrNil toList(ScmObject... list){
		if(list.length==0)
			return ScmNil.NIL;
		ScmPair start=new ScmPair(list[0],ScmNil.NIL),end=start;
		for(int i=1;i<list.length;i++){
			ScmPair newend=new ScmPair(list[i],ScmNil.NIL);
			end.setCdr(newend);
			end=newend;
		}
		return start;
	}
}
