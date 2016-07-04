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
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmComplexPolar implements ScmComplex{
private final ScmReal abs,radius;
	public ScmComplexPolar(ScmReal real,ScmReal imag){
		this.abs=real;
		this.radius=imag;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmComplexPolar&&
				((ScmComplexPolar)obj).abs.equals(abs)&&((ScmComplexPolar)obj).radius.equals(radius);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=31*hash+Objects.hashCode(this.abs);
		hash=31*hash+Objects.hashCode(this.radius);
		return hash;
	}
	@Override
	public String toString(){
		return abs+"exp(i"+radius+")";
	}
	@Override
	public boolean isExact(){
		return abs.isExact()&&radius.isExact();
	}
	@Override
	public String toExternalRepresentation(){
		return abs.toExternalRepresentation()+"@"+radius.toExternalRepresentation();
	}
}
