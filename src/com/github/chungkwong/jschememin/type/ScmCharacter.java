package com.github.chungkwong.jschememin.type;
import java.util.*;
public final class ScmCharacter extends ScmObject implements Comparable<ScmCharacter>{
	static final HashMap<Integer,Integer> caseFoldMap=new HashMap<>();
	int codepoint;
	static{
		Scanner in=new Scanner(ScmCharacter.class.getResourceAsStream("/com/github/chungkwong/jschememin/type/CaseFolding.txt"));
		in.useRadix(16);
		while(in.hasNextInt())
			caseFoldMap.put(in.nextInt(),in.nextInt());
	}
	public ScmCharacter(int codepoint){
		this.codepoint=codepoint;
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
	public int compareTo(ScmCharacter c){
		return codepoint-c.codepoint;
	}
	public int compareToIgnoreCase(ScmCharacter c){
		return foldCase().compareTo(c.foldCase());
	}
	public String toExternalRepresentation(){
		return new String(new int[]{codepoint},0,1);
	}
}
