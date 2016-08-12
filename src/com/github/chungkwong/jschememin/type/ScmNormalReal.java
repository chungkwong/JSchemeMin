/*
 * Copyright (C) 2016 Chan Chung Kwong <1m02math@126.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.github.chungkwong.jschememin.type;

/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class ScmNormalReal extends ScmReal implements Comparable<ScmNormalReal>{
	@Override
	public abstract ScmNormalReal negate();
	@Override
	public abstract ScmNormalReal floor();
	@Override
	public abstract ScmNormalReal ceiling();
	@Override
	public abstract ScmNormalReal truncate();
	@Override
	public abstract ScmNormalReal round();
	public abstract int signum();
	@Override
	public abstract ScmFloatingPointNumber toInExact();
	@Override
	public boolean isZero(){
		return signum()==0;
	}
	@Override
	public boolean isPositive(){
		return signum()>0;
	}
	@Override
	public boolean isNegative(){
		return signum()<0;
	}
	@Override
	public ScmComplex log(){
		return toInExact().log();
	}
	@Override
	public ScmComplex sqrt(){
		return toInExact().sqrt();
	}
	@Override
	public ScmReal exp(){
		return toInExact().exp();
	}
	@Override
	public ScmReal sin(){
		return toInExact().sin();
	}
	@Override
	public ScmReal cos(){
		return toInExact().cos();
	}
	@Override
	public ScmReal getAngle(){
		int sign=signum();
		if(sign>0)
			return ScmInteger.ZERO;
		else if(sign<0)
			return ScmFloatingPointNumber.PI;
		else
			return ScmSpecialReal.POSITIVE_NAN;
	}
	@Override
	public boolean isFinite(){
		return true;
	}
	@Override
	public boolean isInfinite(){
		return false;
	}
	@Override
	public double toDouble(){
		return toInExact().getValue().doubleValue();
	}
}
