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

public abstract class ScmObject{
	public abstract String toExternalRepresentation();
	public abstract boolean isSelfevaluating();
	@Override
	public String toString(){
		return toExternalRepresentation();
	}
	public boolean equalsValue(ScmObject obj){
		return equals(obj);
	}
	public boolean equalsStrict(ScmObject obj){
		return equalsValue(obj);
	}
}
