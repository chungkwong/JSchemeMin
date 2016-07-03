package com.github.chungkwong.jschememin;

public final class Tuple{
	Object car,cdr;
	public Tuple(Object car,Object cdr){
		this.car=car;
		this.cdr=cdr;
	}
	public Object car(){
		return car;
	}
	public Object cdr(){
		return cdr;
	}
	public String toString(){
		return "("+car+","+cdr+")";
	}
}
