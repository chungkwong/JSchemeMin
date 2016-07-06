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
public final class ScmByteVector extends ScmObject{
	private final byte[] vector;
	public ScmByteVector(byte[] vector){
		this.vector=vector;
	}
	public int getLength(){
		return vector.length;
	}
	public ScmInteger get(int index){
		return new ScmInteger(BigInteger.valueOf(index));
	}
	public void set(int index,ScmInteger element){
		int b=element.getValue().intValueExact();
		if(b>=0&&b<256)
			vector[index]=(byte)b;
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append("#u8(");
		if(vector.length>=1)
			buf.append(Byte.toUnsignedInt(vector[0]));
		for(int i=1;i<vector.length;i++)
			buf.append(' ').append(Byte.toUnsignedInt(vector[i]));
		buf.append(')');
		return buf.toString();
	}
	@Override
	public boolean equals(Object obj){
		if(!(obj instanceof ScmByteVector))
			return false;
		ScmByteVector o=(ScmByteVector)obj;
		return Arrays.equals(((ScmByteVector)obj).vector,vector);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=17*hash+Arrays.hashCode(this.vector);
		return hash;
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}
}
