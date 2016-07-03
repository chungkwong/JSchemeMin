package com.github.chungkwong.jschememin.type;

public final class ScmPair<A,D> implements ScmPairOrNil{
	A car;
	D cdr;
	public ScmPair(A car,D cdr){
		this.car=car;
		this.cdr=cdr;
	}
	public A getCar(){
		return car;
	}
	public D getCdr(){
		return cdr;
	}
	public void setCar(A car){
		this.car=car;
	}
	public void setCdr(D cdr){
		this.cdr=cdr;
	}
	public String toString(){
		StringBuilder buf=new StringBuilder();
		buf.append("(");
		buf.append(car);
		Object next=cdr;
		while(next instanceof ScmPair){
			buf.append(",").append(((ScmPair)next).getCar());
			next=((ScmPair)next).getCdr();
		}
		if(!(next instanceof ScmNil))
			buf.append(".").append(next);
		buf.append(")");
		return buf.toString();
	}
}
