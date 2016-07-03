package com.github.chungkwong.jschememin.type;

public final class ScmBoolean{
	public static final ScmBoolean TRUE=new ScmBoolean(true);
	public static final ScmBoolean FALSE=new ScmBoolean(false);
	private final boolean val;
	public ScmBoolean(boolean val){
		this.val=val;
	}
}
