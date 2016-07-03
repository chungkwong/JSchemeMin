package com.github.chungkwong.jschememin;
public final class Rational<T extends Number>{
	T p,q;
	public Rational(T p,T q){
		this.p=p;
		this.q=q;
	}
	public String toString(){
		return p+"/"+q;
	}
}
