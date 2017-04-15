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
import com.github.chungkwong.jschememin.primitive.*;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class SchemeEnvironment extends Environment{
	private Environment parent;
	private final HashMap<ScmSymbol,ScmObject> bindings=new HashMap<>();
	public SchemeEnvironment(boolean repl){
		super(repl);
		this.parent=null;
		bindings.put(DefineLibrary.INSTANCE.getKeyword(),DefineLibrary.INSTANCE);
		bindings.put(Import.INSTANCE.getKeyword(),Import.INSTANCE);
		if(repl)
			Base.INSTANCE.getLibrary().exportTo(this);
	}
	void setParent(Environment parent){
		this.parent=parent;
	}
	public SchemeEnvironment(Environment parent){
		super(parent.isREPL());
		this.parent=parent;
	}
	@Override
	public Optional<ScmObject> getOptional(ScmSymbol id){
		Environment env=getFirstEnvironmentContains(this,id);
		return env!=null?Optional.ofNullable(env.getSelfOptional(id)):Optional.empty();
	}
	@Override
	public ScmObject getSelfOptional(ScmSymbol id){
		return bindings.get(id);
	}
	@Override
	public void set(ScmSymbol id,ScmObject obj){
		Environment env=getFirstEnvironmentContains(this,id);
		if(env!=null){
			env.add(id,obj);
		}else if(isREPL())
			add(id,obj);
	}
	private static Environment getFirstEnvironmentContains(Environment env,ScmSymbol id){
		while(env instanceof SchemeEnvironment){
			if(((SchemeEnvironment)env).bindings.containsKey(id))
				break;
			env=((SchemeEnvironment)env).parent;
		}
		return env;
	}
	@Override
	public void add(ScmSymbol id,ScmObject obj){
		bindings.put(id,obj);
	}
	@Override
	public void remove(ScmSymbol id){
		bindings.remove(id);
	}
}