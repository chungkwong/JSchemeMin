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

public abstract class ScmReal extends ScmComplex{
	public abstract boolean needPlusSign();
	@Override
	public ScmReal getMagnitude(){
		return needPlusSign()?this:this.negate();
	}
	@Override
	public ScmReal getAngle(){
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
	@Override
	public ScmReal getReal(){
		return this;
	}
	@Override
	public ScmReal getImag(){
		return ScmInteger.ZERO;
	}
	public abstract ScmReal negate();
	public abstract ScmReal add(ScmReal num);
	public abstract ScmReal subtract(ScmReal num);
	public abstract ScmReal multiply(ScmReal num);
	public abstract ScmReal divide(ScmReal num);
	public abstract ScmReal sin();
	public abstract ScmReal cos();
	public abstract ScmReal sqrt();
	public abstract int signum();
	@Override
	public boolean isZero(){
		return signum()==0;
	}
}
