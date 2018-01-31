/*
 * Copyright (C) 2015,2016 Chan Chung Kwong
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
package com.github.chungkwong.jschememin;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 * Represents labels for datum
 * @author kwong
 */
public abstract class DatumLabel extends ScmObject implements Token{
	private final String label;
	/**
	 * Create a label
	 * @param label the label name
	 */
	protected DatumLabel(String label){
		this.label=label;
	}
	/**
	 * Get label name
	 * @return
	 */
	public String getLabel(){
		return label;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof DatumLabel&&((DatumLabel)obj).label.equals(label);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=97*hash+Objects.hashCode(getLabel());
		return hash;
	}
	@Override
	public String toExternalRepresentation(){
		throw new UnsupportedOperationException("Not supported yet.");
	}
	@Override
	public boolean isSelfevaluating(){
		throw new UnsupportedOperationException("Not supported yet.");
	}
}
