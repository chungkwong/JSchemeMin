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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class Environment extends ScmObject{
	public static final ScmSymbol UNBOUNDED=new ScmSymbol("unbound");
	private final boolean repl;
	public Environment(boolean repl){
		this.repl=repl;
	}
	public abstract Optional<ScmObject> getOptional(ScmSymbol id);
	public abstract void set(ScmSymbol id,ScmObject obj);
	public abstract void add(ScmSymbol id,ScmObject obj);
	public abstract ScmObject getSelfOptional(ScmSymbol id);
	public abstract void remove(ScmSymbol id);
	public ScmObject get(ScmSymbol id){
		Optional<ScmObject> obj=getOptional(id);
		if(obj.isPresent())
			return obj.get();
		else
			return repl?UNBOUNDED:null;
	}
	public boolean containsKey(ScmSymbol id){
		return getOptional(id).isPresent();
	}
	public void addPrimitiveType(BasicConstruct keyword){
		add(keyword.getKeyword(),keyword);
	}
	public boolean isREPL(){
		return repl;
	}
	@Override
	public String toExternalRepresentation(){
		return "'environment";
		//return '\"'+bindings.toString()+'\"';
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
}
