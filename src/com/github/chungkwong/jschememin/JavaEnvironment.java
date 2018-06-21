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
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
import javax.script.*;
/**
 * Environment backed by ScriptContext
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class JavaEnvironment extends Environment{
	private final ScriptContext parent;
	/**
	 * Create a new environment
	 *
	 * @param parent the backed ScriptContext
	 * @param repl REPL mode or not
	 */
	public JavaEnvironment(ScriptContext parent,boolean repl){
		super(repl);
		this.parent=parent;
	}
	@Override
	public Optional<ScmObject> getOptional(ScmSymbol id){
		return Optional.ofNullable(getSelfOptional(id));
	}
	@Override
	public ScmObject getSelfOptional(ScmSymbol id){
		return ScmJavaObject.toScmObject(parent.getAttribute(id.getValue()));
	}
	@Override
	public void set(ScmSymbol id,ScmObject obj){
		int scope=parent.getAttributesScope(id.getValue());
		if(scope!=-1){
			parent.setAttribute(id.getValue(),ScmJavaObject.toJavaObject(obj),scope);
		}
		if(isREPL()){
			add(id,obj);
		}
	}
	@Override
	public void add(ScmSymbol id,ScmObject obj){
		parent.setAttribute(id.getValue(),ScmJavaObject.toJavaObject(obj),parent.getScopes().get(0));
	}
	@Override
	public void remove(ScmSymbol id){
		int scope=parent.getAttributesScope(id.getValue());
		if(scope!=-1){
			parent.removeAttribute(id.getValue(),scope);
		}
	}
}
