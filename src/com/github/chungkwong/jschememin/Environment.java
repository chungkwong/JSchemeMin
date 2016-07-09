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
package com.github.chungkwong.jschememin;
import com.github.chungkwong.jschememin.lib.*;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Environment{
	private final Environment parent;
	private final boolean repl;
	private final HashMap<ScmSymbol,ScmObject> bindings=new HashMap<>();
	public static final ScmSymbol UNBOUNDED=new ScmSymbol("unbound");
	public Environment(boolean repl){
		this.parent=null;
		this.repl=repl;
		if(repl)
			Base.INSTANCE.getLibrary().exportTo(this);
	}
	public Environment(Environment parent){
		this.parent=parent;
		this.repl=parent.repl;
	}
	public Optional<ScmObject> getOptional(ScmSymbol id){
		Environment env=this;
		while(env!=null){
			ScmObject obj=env.bindings.get(id);
			if(obj!=null)
				return Optional.of(obj);
			env=env.parent;
		}
		return Optional.empty();
	}
	public ScmObject get(ScmSymbol id){
		Optional<ScmObject> obj=getOptional(id);
		if(obj.isPresent())
			return obj.get();
		else
			return repl?UNBOUNDED:null;
	}
	public void set(ScmSymbol id,ScmObject obj){
		Environment env=this;
		while(env!=null){
			if(env.bindings.containsKey(id)){
				env.bindings.put(id,obj);
				return;
			}
			env=env.parent;
		}
		if(repl)
			add(id,obj);
	}
	public void add(ScmSymbol id,ScmObject obj){
		bindings.put(id,obj);
	}
	public void remove(ScmSymbol id){
		bindings.remove(id);
	}
	public void addPrimitiveType(PrimitiveType keyword){
		add(keyword.getKeyword(),keyword);
	}
	public boolean isREPL(){
		return repl;
	}
}