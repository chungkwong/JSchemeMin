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
public final class ScmInteger implements ScmReal{
	public static final ScmInteger ZERO=new ScmInteger(BigInteger.ZERO);
	public static final ScmInteger ONE=new ScmInteger(BigInteger.ONE);
	private final BigInteger value;
	public ScmInteger(BigInteger value){
		this.value=value;
	}
	public static ScmInteger valueOf(long val){
		return new ScmInteger(BigInteger.valueOf(val));
	}
	public ScmInteger negate(){
		return new ScmInteger(value.negate());
	}
	public ScmInteger add(ScmInteger num){
		return new ScmInteger(value.add(num.value));
	}
	public ScmInteger subtract(ScmInteger num){
		return new ScmInteger(value.subtract(num.value));
	}
	public ScmInteger multiply(ScmInteger num){
		return new ScmInteger(value.multiply(num.value));
	}
	public ScmInteger divide(ScmInteger num){
		return new ScmInteger(value.divide(num.value));
	}
	public ScmInteger remainder(ScmInteger num){
		return new ScmInteger(value.mod(num.value));
	}
	public ScmInteger gcd(ScmInteger num){
		return new ScmInteger(value.gcd(num.value));
	}
	public int compareTo(ScmInteger num){
		return value.compareTo(num.value);
	}
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
}
