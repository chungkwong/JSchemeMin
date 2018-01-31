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
/**
 * Represents the type char in Scheme
 * @author kwong
 */
public final class ScmCharacter extends ScmObject implements Comparable<ScmCharacter>,Token{
	static final HashMap<Integer,Integer> CASE_FOLD_MAP=new HashMap<>();
	private final int codepoint;
	static{
		Scanner in=new Scanner(ScmCharacter.class.getResourceAsStream("/com/github/chungkwong/jschememin/type/CaseFolding.txt"),"UTF-8");
		in.useRadix(16);
		while(in.hasNextInt())
			CASE_FOLD_MAP.put(in.nextInt(),in.nextInt());
	}
	/**
	 * Wrap a codepoint
	 * @param codepoint
	 */
	public ScmCharacter(int codepoint){
		this.codepoint=codepoint;
	}
	/**
	 * Convert to a character
	 * @param codePoint
	 * @return
	 */
	public static ScmCharacter getScmCharacter(int codePoint){
		return new ScmCharacter(codePoint);
	}
	/**
	 * Corresponding to the procedure char-upcase in Scheme
	 * @return
	 */
	public ScmCharacter upCase(){
		return new ScmCharacter(Character.toUpperCase(codepoint));
	}
	/**
	 * Corresponding to the procedure char-downcase in Scheme
	 * @return
	 */
	public ScmCharacter downCase(){
		return new ScmCharacter(Character.toLowerCase(codepoint));
	}
	/**
	 * Corresponding to the procedure char-foldcase in Scheme
	 * @return
	 */
	public ScmCharacter foldCase(){
		return CASE_FOLD_MAP.containsKey(codepoint)?new ScmCharacter(CASE_FOLD_MAP.get(codepoint)):this;
	}
	/**
	 * Fold codepoint
	 * @param codepoint
	 * @return
	 */
	public static int toFoldCase(int codepoint){
		return CASE_FOLD_MAP.containsKey(codepoint)?CASE_FOLD_MAP.get(codepoint):codepoint;
	}
	/**
	 * Get the codepoint
	 * @return
	 */
	public int getCodePoint(){
		return codepoint;
	}
	/**
	 * Corresponding to the procedure digit-value in Scheme
	 * @return
	 */
	public int getDigitValue(){
		return Character.getNumericValue(codepoint);
	}
	/**
	 * Corresponding to the procedure alphabetic? in Scheme
	 * @return
	 */
	public boolean isAlphabetic(){
		return Character.isLetter(codepoint);
	}
	/**
	 * Corresponding to the procedure char-numeric? in Scheme
	 * @return
	 */
	public boolean isNumeric(){
		return Character.isDigit(codepoint);
	}
	/**
	 * Corresponding to the procedure char-whitespace? in Scheme
	 * @return
	 */
	public boolean isWhiteSpace(){
		return Character.isWhitespace(codepoint);
	}
	/**
	 * Corresponding to the procedure char-upper-case? in Scheme
	 * @return
	 */
	public boolean isUpperCase(){
		return Character.isUpperCase(codepoint);
	}
	/**
	 * Corresponding to the procedure char-lower-case? in Scheme
	 * @return
	 */
	public boolean isLowerCase(){
		return Character.isLowerCase(codepoint);
	}
	@Override
	public int compareTo(ScmCharacter c){
		return codepoint-c.codepoint;
	}
	/**
	 * Compare to another character after fold
	 * @param c the other character
	 * @return h the result
	 */
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
