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
import java.math.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmFloatingPointNumber extends ScmNormalReal{
	public static final ScmFloatingPointNumber ZERO=new ScmFloatingPointNumber(BigDecimal.ZERO);
	public static final ScmFloatingPointNumber ONE=new ScmFloatingPointNumber(BigDecimal.ONE);
	public static final ScmFloatingPointNumber PI=new ScmFloatingPointNumber(BigDecimal.valueOf(Math.PI));
	public static final ScmFloatingPointNumber HALF=new ScmFloatingPointNumber(BigDecimal.valueOf(0.5));
	private final BigDecimal value;
	public ScmFloatingPointNumber(BigDecimal value){
		this.value=value;
	}
	@Override
	public ScmFloatingPointNumber negate(){
		return new ScmFloatingPointNumber(value.negate());
	}
	public ScmFloatingPointNumber add(ScmFloatingPointNumber num){
		return new ScmFloatingPointNumber(value.add(num.value));
	}
	public ScmFloatingPointNumber subtract(ScmFloatingPointNumber num){
		return new ScmFloatingPointNumber(value.subtract(num.value));
	}
	public ScmFloatingPointNumber multiply(ScmFloatingPointNumber num){
		return new ScmFloatingPointNumber(value.multiply(num.value));
	}
	public ScmReal divide(ScmFloatingPointNumber num){
		if(num.isZero()){
			int sign=signum();
			if(sign>0)
				return ScmSpecialReal.POSITIVE_INF;
			else if(sign<0)
				return ScmSpecialReal.NEGATIVE_INF;
			else
				return ScmSpecialReal.POSITIVE_NAN;
		}else
			try{
				return new ScmFloatingPointNumber(value.divide(num.value));
			}catch(ArithmeticException e){
				return new ScmFloatingPointNumber(value.divide(num.value,MathContext.DECIMAL64));
			}
	}
	@Override
	public ScmReal add(ScmReal num){
		return num instanceof ScmSpecialReal?num:add((ScmFloatingPointNumber)num.toInExact());
	}
	@Override
	public ScmReal subtract(ScmReal num){
		return num instanceof ScmSpecialReal?num.negate():subtract((ScmFloatingPointNumber)num.toInExact());
	}
	@Override
	public ScmReal multiply(ScmReal num){
		return num instanceof ScmSpecialReal?num.multiply(this):multiply((ScmFloatingPointNumber)num.toInExact());
	}
	@Override
	public ScmReal divide(ScmReal num){
		return num instanceof ScmSpecialReal?num.multiply(this):divide((ScmFloatingPointNumber)num.toInExact());
	}
	public int compareTo(ScmFloatingPointNumber num){
		return value.compareTo(num.value);
	}
	@Override
	public int compareTo(ScmNormalReal o){
		if(o instanceof ScmInteger){
			return compareTo((ScmFloatingPointNumber)o.toInExact());
		}else if(o instanceof ScmRational){
			return toScmRational().compareTo((ScmRational)o);
		}else{
			assert o instanceof ScmFloatingPointNumber;
			return compareTo((ScmFloatingPointNumber)o);
		}
	}
	public BigDecimal getValue(){
		return value;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmFloatingPointNumber&&((ScmFloatingPointNumber)obj).value.compareTo(value)==0;
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=67*hash+Objects.hashCode(this.value);
		return hash;
	}
	@Override
	public String toString(){
		return toExternalRepresentation();
	}
	@Override
	public boolean isExact(){
		return false;
	}
	@Override
	public String toExternalRepresentation(int radix){
		return value.toString();
	}
	@Override
	public boolean needPlusSign(){
		return value.signum()>=0;
	}
	@Override
	public boolean isInteger(){
		try{
			value.toBigIntegerExact();
			return true;
		}catch(RuntimeException ex){
			return false;
		}
	}
	@Override
	public ScmInteger toScmInteger(){
		return new ScmInteger(value.toBigIntegerExact());
	}
	@Override
	public boolean isRational(){
		return true;
	}
	@Override
	public ScmRational toScmRational(){
		return new ScmRational(new ScmInteger(value.unscaledValue()),new ScmInteger(BigInteger.TEN.pow(value.scale())));
	}
	@Override
	public ScmReal toExact(){
		return isInteger()?toScmInteger():toScmRational();
	}
	@Override
	public ScmFloatingPointNumber toInExact(){
		return this;
	}
	@Override
	public ScmFloatingPointNumber floor(){
		return new ScmFloatingPointNumber(value.setScale(0,RoundingMode.FLOOR));
	}
	@Override
	public ScmFloatingPointNumber ceiling(){
		return new ScmFloatingPointNumber(value.setScale(0,RoundingMode.CEILING));
	}
	@Override
	public ScmFloatingPointNumber truncate(){
		return new ScmFloatingPointNumber(value.setScale(0,RoundingMode.DOWN));
	}
	@Override
	public ScmFloatingPointNumber round(){
		return new ScmFloatingPointNumber(value.setScale(0,RoundingMode.HALF_EVEN));
	}
	@Override
	public int signum(){
		return value.signum();
	}
	@Override
	public ScmFloatingPointNumber sin(){
		return new ScmFloatingPointNumber(BigDecimal.valueOf(Math.sin(value.doubleValue())));
	}
	@Override
	public ScmFloatingPointNumber cos(){
		return new ScmFloatingPointNumber(BigDecimal.valueOf(Math.cos(value.doubleValue())));
	}
	@Override
	public ScmFloatingPointNumber exp(){
		return new ScmFloatingPointNumber(BigDecimal.valueOf(Math.exp(value.doubleValue())));
	}
	@Override
	public ScmComplex log(){
		int sign=signum();
		if(sign>0)
			return new ScmFloatingPointNumber(BigDecimal.valueOf(Math.log(value.doubleValue())));
		else if(sign<0)
			return new ScmComplexRectangular(negate().log().toScmReal(),PI);
		else
			return ScmSpecialReal.POSITIVE_NAN;
	}
	public static ScmReal valueOf(double d){
		if(Double.isNaN(d))
			return ScmSpecialReal.POSITIVE_NAN;
		else if(Double.isInfinite(d))
			return d>0?ScmSpecialReal.POSITIVE_INF:ScmSpecialReal.NEGATIVE_INF;
		else
			return new ScmFloatingPointNumber(BigDecimal.valueOf(d));
	}
	public static double toDouble(ScmReal d){
		d=d.toInExact();
		if(d instanceof ScmFloatingPointNumber)
			return ((ScmFloatingPointNumber)d).getValue().doubleValue();
		else if(d instanceof ScmSpecialReal.PositiveInf){
			return Double.POSITIVE_INFINITY;
		}else if(d instanceof ScmSpecialReal.NegativeInf){
			return Double.NEGATIVE_INFINITY;
		}else
			return Double.NaN;
	}
}