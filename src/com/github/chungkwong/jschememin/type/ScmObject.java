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
/**
 * Represents objects in scheme
 * @author kwong
 */
public abstract class ScmObject{
	/**
	 * Get the external representation of the object
	 * @return the String
	 */
	public abstract String toExternalRepresentation();
	/**
	 * Check if the object is self-evaluating
	 * @return
	 */
	public abstract boolean isSelfevaluating();
	@Override
	public String toString(){
		return toExternalRepresentation();
	}
	/**
	 * Corresponding to the procedure eqv? in Scheme
	 * @param obj
	 * @return
	 */
	public boolean equalsValue(ScmObject obj){
		return equals(obj);
	}
	/**
	 * Corresponding to the procedure eq? in Scheme
	 * @param obj
	 * @return
	 */
	public boolean equalsStrict(ScmObject obj){
		return equalsValue(obj);
	}
}
