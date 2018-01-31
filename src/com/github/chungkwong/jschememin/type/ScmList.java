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
import java.util.stream.*;
/**
 * Helper for list
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmList{
	/**
	 * Corresponding to the procedure car in Scheme
	 * @param list
	 * @return
	 */
	public static ScmObject first(ScmObject list){
		return ((ScmPair)list).getCar();
	}
	/**
	 * Corresponding to the procedure cadr in Scheme
	 * @param list
	 * @return
	 */
	public static ScmObject second(ScmObject list){
		return ((ScmPair)list).getCadr();
	}
	/**
	 * Corresponding to the procedure caddr in Scheme
	 * @param list
	 * @return
	 */
	public static ScmObject third(ScmObject list){
		return second(((ScmPair)list).getCdr());
	}
	/**
	 * Corresponding to the procedure length in Scheme
	 * @param list
	 * @return
	 */
	public static int getLength(ScmObject list){
		int len=0;
		while(list instanceof ScmPair){
			list=((ScmPair)list).getCdr();
			++len;
		}
		/*if(list!=ScmNil.NIL)
			throw new RuntimeException();*/
		return len;
	}
	/**
	 * Corresponding to the procedure for-each in Scheme
	 * @param list
	 * @param proc
	 */
	public static void forEach(ScmObject list,Consumer<ScmObject> proc){
		ScmObject pair=list;
		while(pair instanceof ScmPair){
			proc.accept(((ScmPair)pair).getCar());
			pair=((ScmPair)pair).getCdr();
		}
		if(pair!=ScmNil.NIL)
			throw new RuntimeException();
	}
	/**
	 * Corresponding to the procedure list? in Scheme
	 * @param list
	 * @return
	 */
	public static boolean isList(ScmObject list){
		HashSet<ScmObject> found=new HashSet<>();
		while(list instanceof ScmPair){
			if(found.contains(list))
				return false;
			found.add(list);
			list=((ScmPair)list).getCdr();
		}
		return list==ScmNil.NIL;
	}
	/**
	 * Build a list
	 * @param list a Java's List
	 * @return
	 */
	public static ScmPairOrNil toList(List<? extends ScmObject> list){
		if(list.isEmpty()){
			return ScmNil.NIL;
		}
		ScmPair start=new ScmPair(list.get(0),ScmNil.NIL);
		ScmPair end=start;
		for(ScmObject o:list.subList(1,list.size())){
			ScmPair newend=new ScmPair(o,ScmNil.NIL);
			end.setCdr(newend);
			end=newend;
		}
		return start;
	}
	/**
	 * Build a list with only one element
	 * @param obj the element
	 * @return
	 */
	public static ScmPair toList(ScmObject obj){
		return new ScmPair(obj,ScmNil.NIL);
	}
	/**
	 * Build a list with two element
	 * @param first
	 * @param second
	 * @return
	 */
	public static ScmPair toList(ScmObject first,ScmObject second){
		return new ScmPair(first,new ScmPair(second,ScmNil.NIL));
	}
	/**
	 * Build a list
	 * @param list
	 * @return
	 */
	public static ScmPairOrNil toList(ScmObject... list){
		if(list.length==0){
			return ScmNil.NIL;
		}
		ScmPair start=new ScmPair(list[0],ScmNil.NIL);
		ScmPair end=start;
		for(int i=1;i<list.length;i++){
			ScmPair newend=new ScmPair(list[i],ScmNil.NIL);
			end.setCdr(newend);
			end=newend;
		}
		return start;
	}
	/**
	 * Corresponding to the procedure make-list in Scheme
	 * @param obj
	 * @param len
	 * @return
	 */
	public static ScmPairOrNil fill(ScmObject obj,int len){
		ScmPairOrNil curr=ScmNil.NIL;
		while(--len>=0)
			curr=new ScmPair(obj,curr);
		return curr;
	}
	/**
	 * Corresponding to the procedure list-ref in Scheme
	 * @param obj
	 * @param index
	 * @return
	 */
	public static ScmObject get(ScmPairOrNil obj,int index){
		while(--index>=0)
			obj=(ScmPairOrNil)((ScmPair)obj).getCdr();
		return ((ScmPair)obj).getCar();
	}
	/**
	 * Corresponding to the procedure reverse in Scheme
	 * @param list
	 * @return
	 */
	public static ScmPairOrNil reverse(ScmPairOrNil list){
		ScmPairOrNil rev=ScmNil.NIL;
		while(list instanceof ScmPair){
			rev=new ScmPair(((ScmPair)list).getCar(),rev);
			list=(ScmPairOrNil)((ScmPair)list).getCdr();
		}
		return rev;
	}
	/**
	 * Corresponding to the procedure append in Scheme
	 * @param lists
	 * @return
	 */
	public static ScmObject append(ScmPairOrNil lists){
		if(lists instanceof ScmNil)
			return ScmNil.NIL;
		ScmListBuilder buf=new ScmListBuilder();
		while(((ScmPair)lists).getCdr() instanceof ScmPair){
			buf.addAll((ScmPairOrNil)((ScmPair)lists).getCar());
			lists=(ScmPairOrNil)((ScmPair)lists).getCdr();
		}
		buf.setLast(((ScmPair)lists).getCar());
		return buf.toList();
	}
	/**
	 * Corresponding to the procedure list-copy in Scheme
	 * @param list
	 * @return
	 */
	public static ScmPairOrNil copy(ScmPairOrNil list){
		if(list instanceof ScmPair){
			ScmPair start=new ScmPair(((ScmPair)list).getCar(),ScmNil.NIL),end=start;
			list=(ScmPairOrNil)((ScmPair)list).getCdr();
			while(list instanceof ScmPair){
				ScmPair toAdd=new ScmPair(((ScmPair)list).getCar(),ScmNil.NIL);
				end.setCdr(toAdd);
				end=toAdd;
				list=(ScmPairOrNil)((ScmPair)list).getCdr();
			}
			return start;
		}else
			return ScmNil.NIL;
	}
	/**
	 * Corresponding to the procedure list-set! in Scheme
	 * @param list
	 * @param index
	 * @param obj
	 */
	public static void set(ScmPair list,int index,ScmObject obj){
		while(--index>=0)
			list=(ScmPair)list.getCdr();
		list.setCar(obj);
	}
	/**
	 * Corresponding to the procedure list-tail in Scheme
	 * @param list
	 * @param index
	 * @return
	 */
	public static ScmObject tail(ScmPairOrNil list,int index){
		while(--index>=0)
			list=(ScmPairOrNil)((ScmPair)list).getCdr();
		return list;
	}
	/**
	 * Cdr until not pair
	 * @param scmPair
	 * @return
	 */
	public static ScmPair getLastListNode(ScmPair scmPair){
		ScmPair node=scmPair;
		while(node.getCdr() instanceof ScmPair){
			node=(ScmPair)node.getCdr();
		}
		return node;
	}
	/**
	 * Build a stream based on a list
	 * @param list the list
	 * @return the stream
	 */
	public static Stream<ScmObject> asStream(ScmPairOrNil list){
		Iterable iter=()->new Iterator() {
			ScmPairOrNil rest=list;
			@Override
			public boolean hasNext(){
				return rest instanceof ScmPair;
			}
			@Override
			public ScmObject next(){
				if(!hasNext())
					throw new NoSuchElementException();
				ScmObject obj=((ScmPair)rest).getCar();
				rest=(ScmPairOrNil)((ScmPair)rest).getCdr();
				return obj;
			}
		};
		return StreamSupport.stream(iter.spliterator(),false);
	}
	/**
	 * A Collector that can be used to build list
	 */
	public static final Collector<ScmObject,ScmListBuilder,ScmPairOrNil> COLLECTOR=new Collector<ScmObject,ScmListBuilder,ScmPairOrNil>() {
		@Override
		public Supplier<ScmListBuilder> supplier(){
			return ()->new ScmListBuilder();
		}
		@Override
		public BiConsumer<ScmListBuilder,ScmObject> accumulator(){
			return (buf,o)->buf.add(o);
		}
		@Override
		public BinaryOperator<ScmListBuilder> combiner(){
			return (buf,buf2)->{buf.addAll(buf);return buf;};
		}
		@Override
		public Function<ScmListBuilder,ScmPairOrNil> finisher(){
			return (buf)->(ScmPairOrNil)buf.toList();
		}
		@Override
		public Set<Collector.Characteristics> characteristics(){
			return Collections.emptySet();
		}
	};
}