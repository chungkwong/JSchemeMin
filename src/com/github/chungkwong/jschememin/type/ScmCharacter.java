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
public final class ScmCharacter extends ScmObject implements Comparable<ScmCharacter>,Token{
	static final HashMap<Integer,Integer> caseFoldMap=new HashMap<>();
	private final int codepoint;
	static{
		Scanner in=new Scanner(ScmCharacter.class.getResourceAsStream("/com/github/chungkwong/jschememin/type/CaseFolding.txt"),"UTF-8");
		in.useRadix(16);
		while(in.hasNextInt())
			caseFoldMap.put(in.nextInt(),in.nextInt());
	}
	public ScmCharacter(int codepoint){
		this.codepoint=codepoint;
	}
	public static ScmCharacter getScmCharacter(int codePoint){
		return new ScmCharacter(codePoint);
	}
	public ScmCharacter upCase(){
		return new ScmCharacter(Character.toUpperCase(codepoint));
	}
	public ScmCharacter downCase(){
		return new ScmCharacter(Character.toLowerCase(codepoint));
	}
	public ScmCharacter foldCase(){
		return caseFoldMap.containsKey(codepoint)?new ScmCharacter(caseFoldMap.get(codepoint)):this;
	}
	public static int toFoldCase(int codepoint){
		return caseFoldMap.containsKey(codepoint)?caseFoldMap.get(codepoint):codepoint;
	}
	public int getCodePoint(){
		return codepoint;
	}
	public int getDigitValue(){
		return Character.getNumericValue(codepoint);
	}
	public boolean isAlphabetic(){
		return Character.isLetter(codepoint);
	}
	public boolean isNumeric(){
		return Character.isDigit(codepoint);
	}
	public boolean isWhiteSpace(){
		return Character.isWhitespace(codepoint);
	}
	public boolean isUpperCase(){
		return Character.isUpperCase(codepoint);
	}
	public boolean isLowerCase(){
		return Character.isLowerCase(codepoint);
	}
	@Override
	public int compareTo(ScmCharacter c){
		return codepoint-c.codepoint;
	}
	public int compareToIgnoreCase(ScmCharacter c){
		return foldCase().compareTo(c.foldCase());
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append("#\\");
		buf.appendCodePoint(codepoint);
		return buf.toString();
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ScmCharacter&&((ScmCharacter)obj).codepoint==codepoint;
	}
	@Override
	public int hashCode(){
		return codepoint;
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}

}
