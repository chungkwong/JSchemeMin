package com.github.chungkwong.jschememin.type;

public final class ScmNil implements ScmPairOrNil{
	public static final ScmNil NIL=new ScmNil();
	private ScmNil(){
	}
	public String toString(){
		return "Nil";
	}
}
