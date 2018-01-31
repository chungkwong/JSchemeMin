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
/**
 * Represents the type complex in Scheme
 * @author kwong
 */
public abstract class ScmComplex extends ScmNumber{
	/**
	 * Corresponding the procedure real-part in Scheme
	 * @return
	 */
	public abstract ScmReal getReal();
	/**
	 * Corresponding the procedure imag-part in Scheme
	 * @return
	 */
	public abstract ScmReal getImag();
	/**
	 * Corresponding the procedure magnitude in Scheme
	 * @return
	 */
	public abstract ScmReal getMagnitude();
	/**
	 * Corresponding the procedure angle in Scheme
	 * @return
	 */
	public abstract ScmReal getAngle();
	@Override
	public boolean isExact(){
		return getReal().isExact()&&getImag().isExact();
	}
	/**
	 * Corresponding the procedure exact in Scheme
	 * @return
	 */
	public ScmComplex toExact(){
		return isExact()?this:new ScmComplexRectangular(getReal().toExact(),getImag().toExact());
	}
	/**
	 * Corresponding the procedure inexact in Scheme
	 * @return
	 */
	public ScmComplex toInExact(){
		return isExact()?new ScmComplexRectangular(getReal().toInExact(),getImag().toInExact()):this;
	}
	/**
	 * Corresponding the procedure log in Scheme
	 * @return
	 */
	public ScmComplex log(){
		return new ScmComplexRectangular(getMagnitude().log().toScmReal(),getAngle());
	}
	/**
	 * Corresponding the procedure exp in Scheme
	 * @return
	 */
	public ScmComplex exp(){
		return new ScmComplexPolar(getReal().exp(),getImag());
	}
	/**
	 * Corresponding the procedure zero? in Scheme
	 * @return
	 */
	public abstract boolean isZero();
	/**
	 * Corresponding the procedure real? in Scheme
	 * @return
	 */
	public boolean isReal(){
		return getImag().isZero();
	}
	/**
	 * Corresponding the procedure ratonal? in Scheme
	 * @return
	 */
	public boolean isRational(){
		return isReal()&&getReal().isRational();
	}
	/**
	 * Corresponding the procedure integer? in Scheme
	 * @return
	 */
	public boolean isInteger(){
		return isReal()&&getReal().isInteger();
	}
	/**
	 * Convert to real, throw a Exception if not possible
	 * @return
	 */
	public ScmReal toScmReal(){
		if(!isReal())
			throw new RuntimeException();
		return getReal();
	}
	/**
	 * Convert to integer, throw a Exception if not possible
	 * @return
	 */
	public ScmInteger toScmInteger(){
		if(!isInteger())
			throw new RuntimeException();
		return getReal().toScmInteger();
	}
	/**
	 * Convert to rational, throw a Exception if not possible
	 * @return
	 */
	public ScmRational toScmRational(){
		if(!isRational())
			throw new RuntimeException();
		return getReal().toScmRational();
	}
	/**
	 * Corresponding the procedure - in Scheme
	 * @return
	 */
	public ScmComplex negate(){
		return new ScmComplexRectangular(getReal().negate(),getImag().negate());
	}
	/**
	 * Corresponding the procedure + in Scheme
	 * @param num
	 * @return
	 */
	public ScmComplex add(ScmComplex num){
		return new ScmComplexRectangular(num.getReal().add(getReal()),num.getImag().add(getImag()));
	}
	/**
	 * Corresponding the procedure - in Scheme
	 * @param num
	 * @return
	 */
	public ScmComplex subtract(ScmComplex num){
		return new ScmComplexRectangular(getReal().subtract(num.getReal()),getImag().subtract(num.getImag()));
	}
	/**
	 * Corresponding the procedure * in Scheme
	 * @param num
	 * @return
	 */
	public ScmComplex multiply(ScmComplex num){
		return new ScmComplexRectangular(getReal().multiply(num.getReal()).subtract(getImag().multiply(num.getImag())),
				getReal().multiply(num.getImag()).add(getImag().multiply(num.getReal())));
		//return new ScmComplexPolar(num.getMagnitude().multiply(getMagnitude()),num.getAngle().add(getAngle()));
	}
	/**
	 * Corresponding the procedure / in Scheme
	 * @param num
	 * @return
	 */
	public ScmComplex divide(ScmComplex num){
		ScmReal d=num.getReal().multiply(num.getReal()).add(num.getImag().multiply(num.getImag()));
		num=new ScmComplexRectangular(num.getReal().divide(d),num.getImag().negate().divide(d));
		return multiply(num);
//return new ScmComplexPolar(getMagnitude().divide(num.getMagnitude()),getAngle().subtract(num.getAngle()));
	}
	/**
	 * Corresponding the procedure square in Scheme
	 * @return
	 */
	public ScmComplex square(){
		return multiply(this);
	}
	/**
	 * Corresponding the procedure log in Scheme
	 * @param base
	 * @return
	 */
	public ScmComplex log(ScmComplex base){
		return log().divide(base.log());
	}
	/**
	 * Corresponding the procedure pow in Scheme
	 * @param e
	 * @return
	 */
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
	/**
	 * Corresponding the procedure sqrt in Scheme
	 * @return
	 */
	public ScmComplex sqrt(){
		return pow(new ScmRational(ScmInteger.ONE,ScmInteger.TWO));
	}
	/**
	 * Corresponding the procedure sin in Scheme
	 * @return
	 */
	public ScmComplex sin(){
		return multiply(I).exp().subtract(negate().multiply(I).exp()).divide(ScmInteger.TWO.multiply(I));
	}
	/**
	 * Corresponding the procedure cos in Scheme
	 * @return
	 */
	public ScmComplex cos(){
		return multiply(I).exp().add(negate().multiply(I).exp()).divide(ScmInteger.TWO);
	}
	/**
	 * Corresponding the procedure tan in Scheme
	 * @return
	 */
	public ScmComplex tan(){
		return sin().divide(cos());
	}
	/**
	 * Corresponding the procedure asin in Scheme
	 * @return
	 */
	public ScmComplex arcsin(){
		return ScmInteger.ONE.subtract(square()).sqrt().add(multiply(I)).log().multiply(I).negate();
	}
	/**
	 * Corresponding the procedure acos in Scheme
	 * @return
	 */
	public ScmComplex arccos(){
		return ScmFloatingPointNumber.PI.divide(ScmInteger.TWO).subtract(arcsin());
	}
	/**
	 * Corresponding the procedure atan in Scheme
	 * @return
	 */
	public ScmComplex arctan(){
		return multiply(I).add(ScmInteger.ONE).log().subtract(multiply(I).negate().add(ScmInteger.ONE).log()).divide(ScmInteger.TWO.multiply(I));
	}
	/**
	 * Convert to int, throw a Exception if not possible
	 * @return
	 */
	public int intValueExact(){
		return toScmInteger().getValue().intValueExact();
	}
	/**
	 * Corresponding the procedure finite? in Scheme
	 * @return
	 */
	public abstract boolean isFinite();
	/**
	 * Corresponding the procedure infinte? in Scheme
	 * @return
	 */
	public abstract boolean isInfinite();
	/**
	 * Corresponding the procedure nan? in Scheme
	 * @return
	 */
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
		if(isReal())
			return toScmReal().hashCode();
		int hash=5;
		hash=37*hash+getReal().hashCode();
		hash=37*hash+getImag().hashCode();
		return hash;
	}
}