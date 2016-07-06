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
public class ScmFloatingPointNumber extends ScmReal{
	public static final ScmFloatingPointNumber ZERO=new ScmFloatingPointNumber(BigDecimal.ZERO);
	public static final ScmFloatingPointNumber ONE=new ScmFloatingPointNumber(BigDecimal.ONE);
	private final BigDecimal value;
	public ScmFloatingPointNumber(BigDecimal value){
		this.value=value;
	}
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
	public ScmFloatingPointNumber divide(ScmFloatingPointNumber num){
		return new ScmFloatingPointNumber(value.divide(num.value));
	}
	public int compareTo(ScmFloatingPointNumber num){
		return value.compareTo(num.value);
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
		return true;
	}
	@Override
	public String toExternalRepresentation(){
		return value.toString();
	}
	@Override
	public boolean needPlusSign(){
		return value.signum()>=0;
	}
	public static final PositiveNaN POSITIVE_NAN=new PositiveNaN();
	public static final NegativeNaN NEGATIVE_NAN=new NegativeNaN();
	public static final PositiveInf POSITIVE_INF=new PositiveInf();
	public static final NegativeInf NEGATIVE_INF=new NegativeInf();
	public static class PositiveNaN extends SpecialValue{
		@Override
		public String toExternalRepresentation(){
			return "+nan.0";
		}
	}
	public static class NegativeNaN extends SpecialValue{
		@Override
		public String toExternalRepresentation(){
			return "-nan.0";
		}
	}
	public static class PositiveInf extends SpecialValue{
		@Override
		public String toExternalRepresentation(){
			return "+inf.0";
		}
	}
	public static class NegativeInf extends SpecialValue{
		@Override
		public String toExternalRepresentation(){
			return "-inf.0";
		}
	}
	static abstract class SpecialValue extends ScmReal{
		@Override
		public boolean isExact(){
			return false;
		}
		@Override
		public boolean needPlusSign(){
			return false;
		}
		@Override
		public String toString(){
			return toExternalRepresentation();
		}
	}
}