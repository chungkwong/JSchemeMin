/*
 * Copyright (C) 2017 Chan Chung Kwong <1m02math@126.com>
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
import javax.script.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class JavaEnvironment extends Environment{
	private final Bindings parent;
	public JavaEnvironment(Bindings parent,boolean repl){
		super(repl);
		this.parent=null;
		parent.put(DefineLibrary.INSTANCE.getKeyword().getValue(),DefineLibrary.INSTANCE);
		parent.put(Import.INSTANCE.getKeyword().getValue(),Import.INSTANCE);
		if(repl)
			Base.INSTANCE.getLibrary().exportTo(this);
	}
	@Override
	public Optional<ScmObject> getOptional(ScmSymbol id){
		String name=id.getValue();
		if(parent.containsKey(name)){
			Object value=parent.get(name);
			return Optional.of(value instanceof ScmObject?(ScmObject)value:new ScmJavaObject(value));
		}else{
			return Optional.empty();
		}
	}
	@Override
	public void set(ScmSymbol id,ScmObject obj){
		if(isREPL()||parent.containsKey(id.getValue()))
			add(id,obj);
	}
	@Override
	public void add(ScmSymbol id,ScmObject obj){
		parent.put(id.getValue(),obj instanceof ScmJavaObject?((ScmJavaObject)obj).getJavaObject():obj);
	}
	@Override
	public void remove(ScmSymbol id){
		parent.remove(id.getValue());
	}
}
