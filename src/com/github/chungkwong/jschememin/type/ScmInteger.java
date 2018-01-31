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
 * Represents the type integer in Scheme
 * @author kwong
 */
public final class ScmInteger extends ScmNormalReal{
	/**
	 * Integer zero
	 */
	public static final ScmInteger ZERO=new ScmInteger(BigInteger.ZERO);
	/**
	 * Integer ONE
	 */
	public static final ScmInteger ONE=new ScmInteger(BigInteger.ONE);
	/**
	 * Integre TWO
	 */
	public static final ScmInteger TWO=new ScmInteger(BigInteger.valueOf(2));
	private final BigInteger value;
	/**
	 * Wrap a long
	 * @param value
	 */
	public ScmInteger(long value){
		this.value=BigInteger.valueOf(value);
	}
	/**
	 * Wrap a BigInteger
	 * @param value
	 */
	public ScmInteger(BigInteger value){
		this.value=value;
	}
	/**
	 * Convert to integer
	 * @param val
	 * @return
	 */
	public static ScmInteger valueOf(long val){
		return new ScmInteger(BigInteger.valueOf(val));
	}
	@Override
	public ScmInteger negate(){
		return new ScmInteger(value.negate());
	}
	ScmInteger add(ScmInteger num){
		return new ScmInteger(value.add(num.value));
	}
	ScmInteger subtract(ScmInteger num){
		return new ScmInteger(value.subtract(num.value));
	}
	ScmInteger multiply(ScmInteger num){
		return new ScmInteger(value.multiply(num.value));
	}
	/**
	 * Corresponding the procedure truncate-quotient in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger divide(ScmInteger num){
		return new ScmInteger(value.divide(num.value));
	}
	/**
	 * Corresponding the procedure truncate-remainder in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger remainder(ScmInteger num){
		return new ScmInteger(value.remainder(num.value));
	}
	/**
	 * Corresponding the procedure truncate/ in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger[] divideAndRemainder(ScmInteger num){
		BigInteger[] qr=value.divideAndRemainder(num.value);
		return new ScmInteger[]{new ScmInteger(qr[0]),new ScmInteger(qr[1])};
	}
	/**
	 * Corresponding the procedure floor-quotient in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger moduloQuotient(ScmInteger num){
		return quotientAndRemainder(num)[0];
	}
	/**
	 * Corresponding the procedure floor-remainder in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger moduloRemainder(ScmInteger num){
		return quotientAndRemainder(num)[1];
	}
	/**
	 * Corresponding the procedure floor/ in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger[] quotientAndRemainder(ScmInteger num){
		BigInteger[] qr=value.divideAndRemainder(num.value);
		if(num.signum()*qr[1].signum()<0){
			qr[0]=qr[0].subtract(BigInteger.ONE);
			qr[1]=qr[1].add(num.value);
		}
		return new ScmInteger[]{new ScmInteger(qr[0]),new ScmInteger(qr[1])};
	}
	/**
	 * Corresponding the procedure gcd? in Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger gcd(ScmInteger num){
		return new ScmInteger(value.gcd(num.value));
	}
	/**
	 * Corresponding the procedure lcm Scheme
	 * @param num
	 * @return
	 */
	public ScmInteger lcm(ScmInteger num){
		return new ScmInteger(value.multiply(num.value).abs().divide(value.gcd(num.value)));
	}
	/**
	 * Corresponding the procedure exact-integer-sqrt in Scheme
	 * @return
	 */
	public ScmInteger[] sqrtExact(){
		BigInteger root=BigInteger.ZERO;
		for(int bit=(value.bitLength()-1)/2;bit>=0;bit--){
			BigInteger cand=root.setBit(bit);
			if(cand.multiply(cand).compareTo(value)<=0){
				root=cand;
			}
		}
		return new ScmInteger[]{new ScmInteger(root),new ScmInteger(value.subtract(root.multiply(root)))};
	}
	int compareTo(ScmInteger num){
		return value.compareTo(num.value);
	}
	@Override
	public int compareTo(ScmNormalReal o){
		if(o instanceof ScmInteger){
			return compareTo((ScmInteger)o);
		}else if(o instanceof ScmRational){
			return toScmRational().compareTo((ScmRational)o);
		}else{
			assert o instanceof ScmFloatingPointNumber;
			return toInExact().compareTo((ScmFloatingPointNumber)o);
		}
	}
	/**
	 * Convert to BigInteger
	 * @return
	 */
	public BigInteger getValue(){
		return value;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmInteger&&((ScmInteger)obj).value.equals(value);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=67*hash+Objects.hashCode(this.value);
		return hash;
	}
	@Override
	public boolean isExact(){
		return true;
	}
	@Override
	public String toExternalRepresentation(int radix){
		return value.toString(radix);
	}
	@Override
	public boolean needPlusSign(){
		return value.signum()>=0;
	}
	@Override
	public int signum(){
		return value.signum();
	}
	@Override
	public boolean isInteger(){
		return true;
	}
	@Override
	public ScmInteger toScmInteger(){
		return this;
	}
	@Override
	public boolean isRational(){
		return true;
	}
	@Override
	public ScmRational toScmRational(){
		return new ScmRational(this,ONE);
	}
	@Override
	public ScmReal add(ScmReal num){
		if(num instanceof ScmInteger){
			return add((ScmInteger)num);
		}else if(num instanceof ScmRational){
			return toScmRational().add(((ScmRational)num));
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
			return subtract((ScmInteger)num);
		}else if(num instanceof ScmRational){
			return toScmRational().subtract(((ScmRational)num));
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
			return multiply((ScmInteger)num);
		}else if(num instanceof ScmRational){
			return toScmRational().multiply(((ScmRational)num));
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
			return new ScmRational(this,(ScmInteger)num);
		}else if(num instanceof ScmRational){
			return toScmRational().divide(((ScmRational)num));
		}else if(num instanceof ScmFloatingPointNumber){
			return toInExact().divide(((ScmFloatingPointNumber)num));
		}else{
			assert num instanceof ScmSpecialReal;
			return num.multiply(this);
		}
	}
	@Override
	public ScmInteger floor(){
		return this;
	}
	@Override
	public ScmInteger ceiling(){
		return this;
	}
	@Override
	public ScmInteger truncate(){
		return this;
	}
	@Override
	public ScmInteger round(){
		return this;
	}
	@Override
	public ScmReal toExact(){
		return this;
	}
	@Override
	public ScmFloatingPointNumber toInExact(){
		return new ScmFloatingPointNumber(new BigDecimal(value));
	}
	/**
	 * Corresponding the procedure even? in Scheme
	 * @return
	 */
	public boolean isEven(){
		return !isOdd();
	}
	/**
	 * Corresponding the procedure odd? in Scheme
	 * @return
	 */
	public boolean isOdd(){
		return value.testBit(0);
	}
}
