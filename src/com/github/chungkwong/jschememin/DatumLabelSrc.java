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
/**
 * Represents labels to be referenced
 * @author kwong
 */
public final class DatumLabelSrc extends DatumLabel{
	/**
	 * Create a label
	 * @param label ID
	 */
	public DatumLabelSrc(String label){
		super(label);
	}
	@Override
	public String toString(){
		return "#"+getLabel()+"=";
	}
}
