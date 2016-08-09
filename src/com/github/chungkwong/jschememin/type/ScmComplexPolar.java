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
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmComplexPolar extends ScmComplex{
	private static final ScmFloatingPointNumber TWOPI=ScmFloatingPointNumber.PI.multiply(ScmInteger.TWO.toInExact());
	private final ScmReal abs,radius;
	public ScmComplexPolar(ScmReal abs,ScmReal radius){
		this.abs=abs;
		if(radius instanceof ScmSpecialReal){
			this.radius=radius;
		}else{
			this.radius=radius.subtract(((ScmFloatingPointNumber)radius.divide(TWOPI)).round().multiply(TWOPI));
		}
	}
	@Override
	public ScmReal getAngle(){
		return radius;
	}
	@Override
	public ScmReal getMagnitude(){
		return abs;
	}
	@Override
	public ScmReal getImag(){
		return abs.multiply(radius.sin());
	}
	@Override
	public ScmReal getReal(){
		return abs.multiply(radius.cos());
	}
	@Override
	public boolean isExact(){
		return abs.isExact()&&radius.isExact();
	}
	@Override
	public String toExternalRepresentation(int radix){
		return abs.toExternalRepresentation(radix)+"@"+radius.toExternalRepresentation(radix);
	}
	@Override
	public boolean isZero(){
		return abs.isZero();
	}
	@Override
	public ScmComplex toExact(){
		return isExact()?this:new ScmComplexPolar(abs.toExact(),radius.toExact());
	}
	@Override
	public ScmComplex toInExact(){
		return isExact()?new ScmComplexPolar(abs.toInExact(),radius.toInExact()):this;
	}
	@Override
	public boolean isFinite(){
		return abs.isFinite();
	}
	@Override
	public boolean isInfinite(){
		return abs.isInfinite();
	}
}
