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
/**
 * Represents the type real in Scheme
 * @author kwong
 */
public abstract class ScmReal extends ScmComplex{
	/**
	 * Check if the value is nonnegative
	 * @return
	 */
	public abstract boolean needPlusSign();
	@Override
	public ScmReal getMagnitude(){
		return needPlusSign()?this:this.negate();
	}
	@Override
	public ScmReal getReal(){
		return this;
	}
	@Override
	public ScmReal getImag(){
		return ScmInteger.ZERO;
	}
	@Override
	public abstract ScmReal negate();
	/**
	 * Corresponding the procedure + in Scheme
	 * @param num
	 * @return
	 */
	public abstract ScmReal add(ScmReal num);
	/**
	 * Corresponding the procedure - in Scheme
	 * @param num
	 * @return
	 */
	public abstract ScmReal subtract(ScmReal num);
	/**
	 * Corresponding the procedure * in Scheme
	 * @param num
	 * @return
	 */
	public abstract ScmReal multiply(ScmReal num);
	/**
	 * Corresponding the procedure / in Scheme
	 * @param num
	 * @return
	 */
	public abstract ScmReal divide(ScmReal num);
	@Override
	public abstract ScmReal toExact();
	@Override
	public abstract ScmReal toInExact();
	@Override
	public abstract ScmReal exp();
	@Override
	public abstract ScmReal sin();
	@Override
	public abstract ScmReal cos();
	/**
	 * Corresponding the procedure floor in Scheme
	 * @return
	 */
	public abstract ScmReal floor();
	/**
	 * Corresponding the procedure ceiling in Scheme
	 * @return
	 */
	public abstract ScmReal ceiling();
	/**
	 * Corresponding the procedure truncate in Scheme
	 * @return
	 */
	public abstract ScmReal truncate();
	/**
	 * Corresponding the procedure round in Scheme
	 * @return
	 */
	public abstract ScmReal round();
	/**
	 * Corresponding the procedure positive? in Scheme
	 * @return
	 */
	public abstract boolean isPositive();
	/**
	 * Corresponding the procedure negative? in Scheme
	 * @return
	 */
	public abstract boolean isNegative();
	/**
	 * Convert to double
	 * @return
	 */
	public abstract double toDouble();
	@Override
	public boolean isNaN(){
		return false;
	}
	/**
	 * Corresponding the procedure max in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static ScmReal max(ScmReal a,ScmReal b){
		if(isNonNaN(a,b))
			if(a.isExact()&&b.isExact())
				return compare(a,b)>0?a:b;
			else
				return compare(a,b)>0?a.toInExact():b.toInExact();
		else
			return ScmSpecialReal.POSITIVE_NAN;
	}
	/**
	 * Corresponding the procedure min in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static ScmReal min(ScmReal a,ScmReal b){
		if(isNonNaN(a,b))
			if(a.isExact()&&b.isExact())
				return compare(a,b)<0?a:b;
			else
				return compare(a,b)<0?a.toInExact():b.toInExact();
		else
			return ScmSpecialReal.POSITIVE_NAN;
	}
	/**
	 * Corresponding the procedure < in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean less(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)<0;
	}
	/**
	 * Corresponding the procedure <= in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean lessEquals(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)<=0;
	}
	/**
	 * Corresponding the procedure > in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean greater(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)>0;
	}
	/**
	 * Corresponding the procedure >= in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean greaterEquals(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)>=0;
	}
	/**
	 * Corresponding the procedure = in Scheme
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equals(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)==0;
	}
	private static boolean isNonNaN(ScmReal a,ScmReal b){
		return a!=ScmSpecialReal.POSITIVE_NAN&&b!=ScmSpecialReal.POSITIVE_NAN;
	}
	private static int compare(ScmReal a,ScmReal b){
		if(a instanceof ScmNormalReal&&b instanceof ScmNormalReal)
			return ((ScmNormalReal)a).compareTo((ScmNormalReal)b);
		else if(a==b)
			return 0;
		else if(a==ScmSpecialReal.NEGATIVE_INF||b==ScmSpecialReal.POSITIVE_INF)
			return -1;
		else// if(a==ScmSpecialReal.POSITIVE_INF||b==ScmSpecialReal.NEGATIVE_INF)
			return 1;
	}
	/**
	 * Convert from double
	 * @param d
	 * @return
	 */
	public static ScmReal valueOf(double d){
		if(Double.isNaN(d))
			return ScmSpecialReal.POSITIVE_NAN;
		else if(Double.isInfinite(d))
			return d>0?ScmSpecialReal.POSITIVE_INF:ScmSpecialReal.NEGATIVE_INF;
		else
			return new ScmFloatingPointNumber(BigDecimal.valueOf(d));
	}
}