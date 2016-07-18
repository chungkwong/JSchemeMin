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
public abstract class ScmComplex extends ScmNumber{
	public abstract ScmReal getReal();
	public abstract ScmReal getImag();
	public abstract ScmReal getMagnitude();
	public abstract ScmReal getAngle();
	public abstract boolean isZero();
	public boolean isReal(){
		return getImag().isZero();
	}
	public boolean isRational(){
		return isReal()&&getReal().isRational();
	}
	public boolean isInteger(){
		return isReal()&&getReal().isInteger();
	}
	public ScmReal toScmReal(){
		if(!isReal())
			throw new RuntimeException();
		return getReal();
	}
	public ScmInteger toScmInteger(){
		if(!isInteger())
			throw new RuntimeException();
		return getReal().toScmInteger();
	}
	public ScmRational toScmRational(){
		if(!isRational())
			throw new RuntimeException();
		return getReal().toScmRational();
	}
	public ScmComplex negate(){
		return new ScmComplexRectangular(getReal().negate(),getImag().negate());
	}
	public ScmComplex add(ScmComplex num){
		return new ScmComplexRectangular(num.getReal().add(getReal()),num.getImag().add(getImag()));
	}
	public ScmComplex subtract(ScmComplex num){
		return new ScmComplexRectangular(num.getReal().subtract(getReal()),num.getImag().subtract(getImag()));
	}
	public ScmComplex multiply(ScmComplex num){
		return new ScmComplexPolar(num.getMagnitude().multiply(getMagnitude()),num.getAngle().add(getAngle()));
	}
	public ScmComplex divide(ScmComplex num){
		return new ScmComplexPolar(num.getMagnitude().divide(getMagnitude()),getAngle().subtract(num.getAngle()));
	}
}
