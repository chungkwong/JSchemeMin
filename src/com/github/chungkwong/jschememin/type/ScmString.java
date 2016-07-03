package com.github.chungkwong.jschememin.type;

public final class ScmString{
	public ScmString(){

	}
	public static String toFoldingCase(String str){
		StringBuilder buf=new StringBuilder();
		for(int i=0;i<str.length();i++)
			buf.append((char)ScmCharacter.toFoldCase(str.charAt(i)));
		return buf.toString();
	}
}
