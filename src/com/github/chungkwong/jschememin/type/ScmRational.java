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
import java.util.*;
public final class ScmRational extends ScmReal{
	private ScmInteger numerator,denominator;
	private boolean simplified=false;
	public ScmRational(ScmInteger numerator,ScmInteger denominator){
		this.numerator=numerator;
		this.denominator=denominator;
	}
	public int compareTo(ScmRational num){
		return numerator.multiply(num.denominator).compareTo(denominator.multiply(num.numerator));
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
		simplify();
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
	public String toExternalRepresentation(){
		return numerator.toExternalRepresentation()+"/"+denominator.toExternalRepresentation();
	}
	@Override
	public boolean needPlusSign(){
		return numerator.needPlusSign()==denominator.needPlusSign();
	}
}
