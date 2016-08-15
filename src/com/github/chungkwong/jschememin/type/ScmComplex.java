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
import static com.github.chungkwong.jschememin.type.ScmComplexRectangular.I;
public abstract class ScmComplex extends ScmNumber{
	public abstract ScmReal getReal();
	public abstract ScmReal getImag();
	public abstract ScmReal getMagnitude();
	public abstract ScmReal getAngle();
	@Override
	public boolean isExact(){
		return getReal().isExact()&&getImag().isExact();
	}
	public ScmComplex toExact(){
		return isExact()?this:new ScmComplexRectangular(getReal().toExact(),getImag().toExact());
	}
	public ScmComplex toInExact(){
		return isExact()?new ScmComplexRectangular(getReal().toInExact(),getImag().toInExact()):this;
	}
	public ScmComplex log(){
		return new ScmComplexRectangular(getMagnitude().log().toScmReal(),getAngle());
	}
	public ScmComplex exp(){
		return new ScmComplexPolar(getReal().exp(),getImag());
	}
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
		return new ScmComplexRectangular(getReal().subtract(num.getReal()),getImag().subtract(num.getImag()));
	}
	public ScmComplex multiply(ScmComplex num){
		return new ScmComplexPolar(num.getMagnitude().multiply(getMagnitude()),num.getAngle().add(getAngle()));
	}
	public ScmComplex divide(ScmComplex num){
		return new ScmComplexPolar(getMagnitude().divide(num.getMagnitude()),getAngle().subtract(num.getAngle()));
	}
	public ScmComplex square(){
		return multiply(this);
	}
	public ScmComplex log(ScmComplex base){
		return log().divide(base.log());
	}
	public ScmComplex pow(ScmComplex e){
		if(isZero()){
			if(e.isZero())
				return isExact()&&e.isExact()?ScmInteger.ONE:ScmInteger.ONE.toInExact();
			else if(e.getReal().isPositive())
				return isExact()&&e.isExact()?ScmInteger.ZERO:ScmInteger.ZERO.toInExact();
			else
				throw new RuntimeException();
		}else
			return log().multiply(e).exp();
	}
	public ScmComplex sqrt(){
		return pow(new ScmRational(ScmInteger.ONE,ScmInteger.TWO));
	}
	public ScmComplex sin(){
		return multiply(I).exp().subtract(negate().multiply(I).exp()).divide(ScmInteger.TWO.multiply(I));
	}
	public ScmComplex cos(){
		return multiply(I).exp().add(negate().multiply(I).exp()).divide(ScmInteger.TWO);
	}
	public ScmComplex tan(){
		return sin().divide(cos());
	}
	public ScmComplex arcsin(){
		return ScmInteger.ONE.subtract(square()).sqrt().add(multiply(I)).log().multiply(I).negate();
	}
	public ScmComplex arccos(){
		return ScmFloatingPointNumber.PI.divide(ScmInteger.TWO).subtract(arcsin());
	}
	public ScmComplex arctan(){
		return multiply(I).add(ScmInteger.ONE).log().subtract(multiply(I).negate().add(ScmInteger.ONE).log()).divide(ScmInteger.TWO.multiply(I));
	}
	public int intValueExact(){
		return toScmInteger().getValue().intValueExact();
	}
	public abstract boolean isFinite();
	public abstract boolean isInfinite();
	public boolean isNaN(){
		return getReal().isNaN()||getImag().isNaN();
	}
	@Override
	public String toExternalRepresentation(){
		return toExternalRepresentation(10);
	}
	public abstract String toExternalRepresentation(int radix);
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmComplex&&isExact()==((ScmComplex)obj).isExact()&&
				ScmReal.equals(getReal(),((ScmComplex)obj).getReal())&&ScmReal.equals(getImag(),((ScmComplex)obj).getImag());
	}
	@Override
	public int hashCode(){
		int hash=5;
		hash=37*hash+Double.hashCode(getReal().toDouble());
		hash=37*hash+Double.hashCode(getImag().toDouble());
		return hash;
	}
}