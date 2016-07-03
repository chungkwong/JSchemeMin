package com.github.chungkwong.jschememin;

public final class Token{
	private final String id;
	private Token(String id){
		this.id=id;
	}
	public static Token getToken(String id){
		return new Token(id);
	}
	public String toString(){
		return id;
	}
}
