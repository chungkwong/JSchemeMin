package com.github.chungkwong.jschememin;

public final class Identifier{
	private final String id;
	public Identifier(String id){
		this.id=id;
	}
	@Override
	public String toString(){
		return id;
	}
}
