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
import java.nio.*;
import java.nio.charset.*;
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
		return new ScmInteger(BigInteger.valueOf(vector[index]));
	}
	public ScmByteVector set(int index,ScmComplex element){
		vector[index]=byteValueExact(element);
		return this;
	}
	public ScmByteVector copy(int start,int end){
		return new ScmByteVector(Arrays.copyOfRange(vector,start,end));
	}
	public ScmByteVector copyTo(ScmByteVector to,int at,int start,int end){
		while(start<end)
			to.vector[at++]=vector[start++];
		return to;
	}
	public static ScmByteVector append(ScmPairOrNil list){
		int len=ScmList.asStream(list).mapToInt((bv)->((ScmByteVector)bv).getLength()).sum();
		byte[] data=new byte[len];
		int i=0;
		while(list instanceof ScmPair){
			for(byte b:((ScmByteVector)((ScmPair)list).getCar()).vector)
				data[i++]=b;
			list=(ScmPairOrNil)((ScmPair)list).getCdr();
		}
		return new ScmByteVector(data);
	}
	private static byte byteValueExact(ScmComplex i){
		int b=i.intValueExact();
		if(b>=0&&b<256)
			return (byte)b;
		else
			throw new RuntimeException();
	}
	public byte[] getByteArray(){
		return vector;
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
	public static ScmByteVector fill(ScmInteger byt,int size){
		int b=byteValueExact(byt);
		byte[] data=new byte[size];
		Arrays.fill(data,(byte)b);
		return new ScmByteVector(data);
	}
	@Override
	public boolean equals(Object obj){
		if(!(obj instanceof ScmByteVector))
			return false;
		ScmByteVector o=(ScmByteVector)obj;
		return Arrays.equals(((ScmByteVector)obj).vector,vector);
	}
	@Override
	public boolean equalsValue(ScmObject obj){
		return this==obj;
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
	public static ScmByteVector toByteVector(ScmPairOrNil list){
		int len=ScmList.getLength(list);
		byte[] data=new byte[len];
		for(int i=0;i<len;i++){
			data[i]=byteValueExact((ScmComplex)((ScmPair)list).getCar());
			list=(ScmPairOrNil)((ScmPair)list).getCdr();
		}
		return new ScmByteVector(data);
	}
	public ScmString decodeUTF8(int start,int end){
		return new ScmString(StandardCharsets.UTF_8.decode(ByteBuffer.wrap(vector,start,end-start)).toString());
	}
}
