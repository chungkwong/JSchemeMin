/*
 * Copyright (C) 2016 Chan Chung Kwong
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 */
package com.github.chungkwong.jschememin.type;
import com.github.chungkwong.jschememin.*;
public final class ScmBoolean extends ScmObject implements Token{
	public static final ScmBoolean TRUE=new ScmBoolean(true);
	public static final ScmBoolean FALSE=new ScmBoolean(false);
	private final boolean val;
	public ScmBoolean(boolean val){
		this.val=val;
	}
	public boolean isTrue(){
		return val;
	}
	@Override
	public boolean equals(Object obj){
		return this==obj;
	}
	@Override
	public int hashCode(){
		return this.val?1:0;
	}
	@Override
	public String toExternalRepresentation(){
		return val?"#t":"#f";
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}
	public static ScmBoolean valueOf(boolean b){
		return b?ScmBoolean.TRUE:ScmBoolean.FALSE;
	}
}
