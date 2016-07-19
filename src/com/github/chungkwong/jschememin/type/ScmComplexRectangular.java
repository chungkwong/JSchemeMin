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
public class ScmComplexRectangular extends ScmComplex{
	private final ScmReal real,imag;
	public ScmComplexRectangular(ScmReal real,ScmReal imag){
		this.real=real;
		this.imag=imag;
	}
	@Override
	public ScmReal getReal(){
		return real;
	}
	@Override
	public ScmReal getImag(){
		return imag;
	}
	@Override
	public ScmReal getMagnitude(){
		return real.multiply(real).add(imag.multiply(imag)).sqrt();
	}
	@Override
	public ScmReal getAngle(){
		throw new UnsupportedOperationException("Not supported yet."); //TODO
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmComplexRectangular
				&&((ScmComplexRectangular)obj).real.equals(real)&&((ScmComplexRectangular)obj).imag.equals(imag);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=31*hash+Objects.hashCode(this.real);
		hash=31*hash+Objects.hashCode(this.imag);
		return hash;
	}
	@Override
	public boolean isExact(){
		return real.isExact()&&imag.isExact();
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append(real.toExternalRepresentation());
		if(imag.needPlusSign())
			buf.append('+');
		buf.append(imag.toExternalRepresentation());
		buf.append("i");
		return buf.toString();
	}
	@Override
	public boolean isZero(){
		return real.isZero()&&imag.isZero();
	}
	@Override
	public ScmComplex toExact(){
		return isExact()?this:new ScmComplexRectangular(real.toExact(),imag.toExact());
	}
	@Override
	public ScmComplex toInExact(){
		return isExact()?new ScmComplexRectangular(real.toInExact(),imag.toInExact()):this;
	}
}
