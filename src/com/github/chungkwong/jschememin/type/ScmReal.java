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
public abstract class ScmReal extends ScmComplex{
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
	public abstract ScmReal add(ScmReal num);
	public abstract ScmReal subtract(ScmReal num);
	public abstract ScmReal multiply(ScmReal num);
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
	public abstract ScmReal floor();
	public abstract ScmReal ceiling();
	public abstract ScmReal truncate();
	public abstract ScmReal round();
	public abstract boolean isPositive();
	public abstract boolean isNegative();
	public abstract double toDouble();
	@Override
	public boolean isNaN(){
		return false;
	}
	public static ScmReal max(ScmReal a,ScmReal b){
		if(isNonNaN(a,b))
			if(a.isExact()&&b.isExact())
				return compare(a,b)>0?a:b;
			else
				return compare(a,b)>0?a.toInExact():b.toInExact();
		else
			return ScmSpecialReal.POSITIVE_NAN;
	}
	public static ScmReal min(ScmReal a,ScmReal b){
		if(isNonNaN(a,b))
			if(a.isExact()&&b.isExact())
				return compare(a,b)<0?a:b;
			else
				return compare(a,b)<0?a.toInExact():b.toInExact();
		else
			return ScmSpecialReal.POSITIVE_NAN;
	}
	public static boolean less(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)<0;
	}
	public static boolean lessEquals(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)<=0;
	}
	public static boolean greater(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)>0;
	}
	public static boolean greaterEquals(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)>=0;
	}
	public static boolean equals(ScmReal a,ScmReal b){
		return isNonNaN(a,b)&&compare(a,b)==0;
	}
	public static boolean isNonNaN(ScmReal a,ScmReal b){
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
	public static ScmReal valueOf(double d){
		if(Double.isNaN(d))
			return ScmSpecialReal.POSITIVE_NAN;
		else if(Double.isInfinite(d))
			return d>0?ScmSpecialReal.POSITIVE_INF:ScmSpecialReal.NEGATIVE_INF;
		else
			return new ScmFloatingPointNumber(BigDecimal.valueOf(d));
	}
}