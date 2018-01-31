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
 * Represents the type rational in Scheme
 * @author kwong
 */
public final class ScmRational extends ScmNormalReal{
	private ScmInteger numerator,denominator;
	private boolean simplified=false;
	/**
	 * Create a rational number
	 * @param numerator
	 * @param denominator
	 */
	public ScmRational(ScmInteger numerator,ScmInteger denominator){
		int sign=denominator.signum();
		if(sign<0){
			this.numerator=numerator.negate();
			this.denominator=denominator.negate();
		}else if(sign>0){
			this.numerator=numerator;
			this.denominator=denominator;
		}else
			throw new ArithmeticException("divided by zero");
	}
	/**
	 * Corresponding procedure denominator in Scheme
	 * @return
	 */
	public ScmInteger getDenominator(){
		simplify();
		return denominator;
	}
	/**
	 * Corresponding procedure numerator in Scheme
	 * @return
	 */
	public ScmInteger getNumerator(){
		simplify();
		return numerator;
	}
	int compareTo(ScmRational num){
		return numerator.multiply(num.denominator).compareTo(denominator.multiply(num.numerator));
	}
	@Override
	public int compareTo(ScmNormalReal o){
		if(o instanceof ScmInteger){
			return compareTo(o.toScmRational());
		}else if(o instanceof ScmRational){
			return compareTo((ScmRational)o);
		}else{
			assert o instanceof ScmFloatingPointNumber;
			return compareTo(o.toScmRational());
		}
	}
	private void simplify(){
		if(simplified||numerator.equals(ScmInteger.ZERO))
			return;
		ScmInteger gcd=numerator.gcd(denominator);
		numerator=numerator.divide(gcd);
		denominator=denominator.divide(gcd);
		simplified=true;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmRational&&((ScmRational)obj).compareTo(this)==0;
	}
	@Override
	public int hashCode(){
		if(isInteger())
			return numerator.hashCode();
		int hash=5;
		hash=97*hash+Objects.hashCode(this.numerator);
		hash=97*hash+Objects.hashCode(this.denominator);
		return hash;
	}
	@Override
	public boolean isExact(){
		return true;
	}
	@Override
	public String toExternalRepresentation(int radix){
		return numerator.toExternalRepresentation(radix)+"/"+denominator.toExternalRepresentation(radix);
	}
	@Override
	public boolean needPlusSign(){
		return numerator.needPlusSign()==denominator.needPlusSign();
	}
	@Override
	public ScmRational negate(){
		return new ScmRational(numerator.negate(),denominator);
	}
	ScmRational add(ScmRational num){
		return new ScmRational(numerator.multiply(num.denominator).add(denominator.multiply(num.numerator)),denominator.multiply(num.denominator));
	}
	ScmRational subtract(ScmRational num){
		return new ScmRational(numerator.multiply(num.denominator).subtract(denominator.multiply(num.numerator)),denominator.multiply(num.denominator));
	}
	ScmRational multiply(ScmRational num){
		return new ScmRational(numerator.multiply(num.numerator),denominator.multiply(num.denominator));
	}
	ScmRational divide(ScmRational num){
		return new ScmRational(numerator.multiply(num.denominator),denominator.multiply(num.numerator));
	}
	@Override
	public boolean isInteger(){
		simplify();
		return denominator.equals(ScmInteger.ONE);
	}
	@Override
	public ScmInteger toScmInteger(){
		return numerator;
	}
	@Override
	public boolean isRational(){
		return true;
	}
	@Override
	public ScmRational toScmRational(){
		return this;
	}
	@Override
	public ScmReal toExact(){
		return this;
	}
	@Override
	public ScmFloatingPointNumber toInExact(){
		return new ScmFloatingPointNumber(new BigDecimal(numerator.getValue())
				.divide(new BigDecimal(denominator.getValue())));
	}
	@Override
	public ScmReal add(ScmReal num){
		if(num instanceof ScmInteger){
			return add(((ScmInteger)num).toScmRational());
		}else if(num instanceof ScmRational){
			return add(((ScmRational)num));
		}else if(num instanceof ScmFloatingPointNumber){
			return toInExact().add(num);
		}else{
			assert num instanceof ScmSpecialReal;
			return num;
		}
	}
	@Override
	public ScmReal subtract(ScmReal num){
		if(num instanceof ScmInteger){
			return subtract(((ScmInteger)num).toScmRational());
		}else if(num instanceof ScmRational){
			return subtract(((ScmRational)num));
		}else if(num instanceof ScmFloatingPointNumber){
			return toInExact().subtract(num);
		}else{
			assert num instanceof ScmSpecialReal;
			return num.negate();
		}
	}
	@Override
	public ScmReal multiply(ScmReal num){
		if(num instanceof ScmInteger){
			return multiply(((ScmInteger)num).toScmRational());
		}else if(num instanceof ScmRational){
			return multiply(((ScmRational)num));
		}else if(num instanceof ScmFloatingPointNumber){
			return toInExact().multiply(num);
		}else{
			assert num instanceof ScmSpecialReal;
			return num.multiply(this);
		}
	}
	@Override
	public ScmReal divide(ScmReal num){
		if(num instanceof ScmInteger){
			return divide(((ScmInteger)num).toScmRational());
		}else if(num instanceof ScmRational){
			return divide(((ScmRational)num));
		}else if(num instanceof ScmFloatingPointNumber){
			return toInExact().divide(num);
		}else{
			assert num instanceof ScmSpecialReal;
			return num.multiply(this);
		}
	}
	@Override
	public ScmInteger floor(){
		return numerator.moduloQuotient(denominator);
	}
	@Override
	public ScmInteger ceiling(){
		ScmInteger q=numerator.moduloQuotient(denominator);
		ScmInteger r=numerator.moduloRemainder(denominator);
		return r.signum()==0?q:q.add(ScmInteger.ONE);
	}
	@Override
	public ScmInteger truncate(){
		return numerator.divide(denominator);
	}
	@Override
	public ScmInteger round(){
		ScmInteger q=numerator.moduloQuotient(denominator);
		ScmInteger r=numerator.moduloRemainder(denominator);
		int sign=r.multiply(ScmInteger.TWO).compareTo(denominator);
		if(sign<0)
			return q;
		else if(sign>0||q.isOdd())
			return q.add(ScmInteger.ONE);
		else
			return q;
	}
	@Override
	public int signum(){
		return numerator.signum();
	}
	/**
	 * Corresponding procedure rationalize in Scheme
	 * @param x
	 * @param error
	 * @return
	 */
	public static ScmReal rationalize(ScmReal x,ScmReal error){
		if(x instanceof ScmNormalReal)
			return rationalize((ScmNormalReal)x,error);
		else
			throw new RuntimeException();
	}
	private static ScmReal rationalize(ScmNormalReal x,ScmReal error){
		if(x.signum()<0)
			return rationalize(x.negate(),error).negate();
		ScmInteger d=ScmInteger.ONE;
		ScmInteger n=x.floor().toScmInteger();
		while(x.multiply(d).subtract(n).subtract(error.multiply(d)).isPositive()){
			n=n.add(ScmInteger.ONE);
			if(!n.subtract(x.multiply(d)).subtract(error.multiply(d)).isPositive())
				break;
			d=d.add(ScmInteger.ONE);
			n=x.multiply(d).floor().toScmInteger();
		}
		return new ScmRational(n,d);
	}
}