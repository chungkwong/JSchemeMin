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
	private static final ScmInteger THREE=new ScmInteger(3);
	private final ScmReal abs,radius;
	public ScmComplexPolar(ScmReal abs,ScmReal radius){
		this.abs=abs;
		if(abs.isZero()){
			this.radius=ScmInteger.ZERO;
		}else if(radius instanceof ScmSpecialReal||ScmReal.lessEquals(radius.getMagnitude(),THREE)){
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
		return radius.equals(ScmInteger.ZERO)?ScmInteger.ZERO:abs.multiply(radius.sin());
	}
	@Override
	public ScmReal getReal(){
		return radius.equals(ScmInteger.ZERO)?abs:abs.multiply(radius.cos());
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
	public boolean isFinite(){
		return abs.isFinite();
	}
	@Override
	public boolean isInfinite(){
		return abs.isInfinite();
	}
}
