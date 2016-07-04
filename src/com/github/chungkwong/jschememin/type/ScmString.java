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
import com.github.chungkwong.jschememin.*;
import java.util.*;
public final class ScmString implements Token,ScmObject{
	private final String val;
	public ScmString(String val){
		this.val=val;
	}
	public static String toFoldingCase(String str){
		StringBuilder buf=new StringBuilder();
		for(int i=0;i<str.length();i++)
			buf.append((char)ScmCharacter.toFoldCase(str.charAt(i)));
		return buf.toString();
	}
	public String getValue(){
		return val;
	}
	@Override
	public String toString(){
		return val;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmString&&((ScmString)obj).val.equals(val);
	}
	@Override
	public int hashCode(){
		return Objects.hashCode(this.val);
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append("\"");
		val.codePoints().forEach((c)->{
			if(c=='\"'||c=='\\')
				buf.append('\"');
			buf.append(c);
		});
		buf.append("\"");
		return buf.toString();
	}

}
