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
/**
 * Represents the type bytevector in Scheme
 * @author kwong
 */
public final class ScmByteVector extends ScmObject{
	private final byte[] vector;
	/**
	 * Wrap a array of byte
	 * @param vector
	 */
	public ScmByteVector(byte[] vector){
		this.vector=vector;
	}
	/**
	 * Get the length of the vector
	 * @return
	 */
	public int getLength(){
		return vector.length;
	}
	/**
	 * Corresponding to the procedure bytevector-u8-ref of Scheme
	 * @param index
	 * @return
	 */
	public ScmInteger get(int index){
		return new ScmInteger(BigInteger.valueOf(vector[index]));
	}
	/**
	 * Corresponding to the procedure bytevector-u8-set! of Scheme
	 * @param index
	 * @param element
	 * @return
	 */
	public ScmByteVector set(int index,ScmComplex element){ 
		vector[index]=byteValueExact(element);
		return this;
	}
	/**
	 * Corresponding to the procedure bytevector-copy of Scheme
	 * @param start
	 * @param end
	 * @return
	 */
	public ScmByteVector copy(int start,int end){
		return new ScmByteVector(Arrays.copyOfRange(vector,start,end));
	}
	/**
	 * Corresponding to the procedure bytevector-copy! of Scheme
	 * @param to
	 * @param at
	 * @param start
	 * @param end
	 * @return
	 */
	public ScmByteVector copyTo(ScmByteVector to,int at,int start,int end){
		while(start<end)
			to.vector[at++]=vector[start++];
		return to;
	}
	/**
	 * Corresponding to the procedure bytevector-append of Scheme
	 * @param list
	 * @return
	 */
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
	/**
	 * Get the value in array
	 * @return
	 */
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
	/**
	 * Build a bytevector which all elements are the same
	 * @param byt the element
	 * @param size the length
	 * @return the new bytevector
	 */
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
	/**
	 * Convert to bytevector
	 * @param list
	 * @return
	 */
	public static ScmByteVector toByteVector(ScmPairOrNil list){
		int len=ScmList.getLength(list);
		byte[] data=new byte[len];
		for(int i=0;i<len;i++){
			data[i]=byteValueExact((ScmComplex)((ScmPair)list).getCar());
			list=(ScmPairOrNil)((ScmPair)list).getCdr();
		}
		return new ScmByteVector(data);
	}
	/**
	 * Decode the data using UTF-8
	 * @param start
	 * @param end
	 * @return
	 */
	public ScmString decodeUTF8(int start,int end){
		return new ScmString(StandardCharsets.UTF_8.decode(ByteBuffer.wrap(vector,start,end-start)).toString());
	}
}
