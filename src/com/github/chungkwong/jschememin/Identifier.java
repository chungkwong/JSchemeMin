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
package com.github.chungkwong.jschememin;
import java.util.*;
public final class Identifier implements Token{
	private final String id;
	public Identifier(String id){
		this.id=id;
	}
	@Override
	public String toString(){
		return id;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof Identifier&&((Identifier)obj).id.equals(id);
	}
	@Override
	public int hashCode(){
		return Objects.hashCode(this.id);
	}
}
