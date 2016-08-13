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
public abstract class ScmSpecialReal extends ScmReal{
	public static final PositiveNaN POSITIVE_NAN=new PositiveNaN();
	public static final PositiveInf POSITIVE_INF=new PositiveInf();
	public static final NegativeInf NEGATIVE_INF=new NegativeInf();
	@Override
	public boolean isExact(){
		return false;
	}
	@Override
	public ScmReal toExact(){
		throw new RuntimeException();
	}
	@Override
	public ScmReal toInExact(){
		return this;
	}
	@Override
	public boolean needPlusSign(){
		return false;
	}
	@Override
	public boolean isRational(){
		return false;
	}
	@Override
	public boolean isInteger(){
		return false;
	}
	@Override
	public boolean isZero(){
		return false;
	}
	@Override
	public ScmReal sin(){
		return POSITIVE_NAN;
	}
	@Override
	public ScmReal cos(){
		return POSITIVE_NAN;
	}

	@Override
	public String toString(){
		return toExternalRepresentation();
	}
	@Override
	public ScmReal floor(){
		return this;
	}
	@Override
	public ScmReal ceiling(){
		return this;
	}
	@Override
	public ScmReal truncate(){
		return this;
	}
	@Override
	public ScmReal round(){
		return this;
	}
	public static class PositiveNaN extends ScmSpecialReal{
		@Override
		public String toExternalRepresentation(int radix){
			return "+nan.0";
		}
		@Override
		public ScmReal negate(){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal add(ScmReal num){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal subtract(ScmReal num){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal multiply(ScmReal num){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal divide(ScmReal num){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal sqrt(){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal exp(){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal log(){
			return POSITIVE_NAN;
		}
		@Override
		public ScmReal getAngle(){
			return POSITIVE_NAN;
		}
		@Override
		public boolean isFinite(){
			return false;
		}
		@Override
		public boolean isInfinite(){
			return false;
		}
		@Override
		public boolean isNaN(){
			return true;
		}
		@Override
		public boolean isPositive(){
			return false;
		}
		@Override
		public boolean isNegative(){
			return false;
		}
		@Override
		public double toDouble(){
			return Double.NaN;
		}
	}
	public static class PositiveInf extends ScmSpecialReal{
		@Override
		public String toExternalRepresentation(int radix){
			return "+inf.0";
		}
		@Override
		public ScmReal negate(){
			return NEGATIVE_INF;
		}
		@Override
		public ScmReal add(ScmReal num){
			return num instanceof PositiveNaN||num instanceof NegativeInf?POSITIVE_NAN:POSITIVE_INF;
		}
		@Override
		public ScmReal subtract(ScmReal num){
			return num instanceof PositiveNaN||num instanceof PositiveInf?POSITIVE_NAN:POSITIVE_INF;
		}
		@Override
		public ScmReal multiply(ScmReal num){
			if(num instanceof ScmNormalReal)
				if(num.isZero())
					return POSITIVE_NAN;
				else
					return ((ScmNormalReal)num).signum()>0?POSITIVE_INF:NEGATIVE_INF;
			else if(num instanceof PositiveNaN)
				return POSITIVE_NAN;
			else if(num instanceof PositiveInf)
				return POSITIVE_INF;
			else
				return NEGATIVE_INF;

		}
		@Override
		public ScmReal divide(ScmReal num){
			return multiply(num);
		}
		@Override
		public ScmReal exp(){
			return POSITIVE_INF;
		}
		@Override
		public ScmReal log(){
			return POSITIVE_INF;
		}
		@Override
		public ScmReal getAngle(){
			return ScmInteger.ZERO;
		}
		@Override
		public boolean isFinite(){
			return false;
		}
		@Override
		public boolean isInfinite(){
			return true;
		}
		@Override
		public boolean isPositive(){
			return true;
		}
		@Override
		public boolean isNegative(){
			return false;
		}
		@Override
		public double toDouble(){
			return Double.POSITIVE_INFINITY;
		}
	}
	public static class NegativeInf extends ScmSpecialReal{
		@Override
		public String toExternalRepresentation(int radix){
			return "-inf.0";
		}
		@Override
		public ScmReal negate(){
			return POSITIVE_INF;
		}
		@Override
		public ScmReal add(ScmReal num){
			return num instanceof PositiveNaN||num instanceof PositiveInf?POSITIVE_NAN:NEGATIVE_INF;
		}
		@Override
		public ScmReal subtract(ScmReal num){
			return num instanceof PositiveNaN||num instanceof NegativeInf?POSITIVE_NAN:NEGATIVE_INF;
		}
		@Override
		public ScmReal multiply(ScmReal num){
			if(num instanceof ScmNormalReal)
				if(num.isZero())
					return POSITIVE_NAN;
				else
					return ((ScmNormalReal)num).signum()>0?NEGATIVE_INF:POSITIVE_INF;
			else if(num instanceof PositiveNaN)
				return POSITIVE_NAN;
			else if(num instanceof PositiveInf)
				return NEGATIVE_INF;
			else
				return POSITIVE_INF;

		}
		@Override
		public ScmReal divide(ScmReal num){
			return multiply(num);
		}
		@Override
		public ScmReal exp(){
			return ScmInteger.ZERO;
		}
		@Override
		public ScmComplex log(){
			return new ScmComplexRectangular(POSITIVE_INF,ScmFloatingPointNumber.PI);
		}
		@Override
		public ScmReal getAngle(){
			return ScmFloatingPointNumber.PI;
		}
		@Override
		public boolean isFinite(){
			return false;
		}
		@Override
		public boolean isInfinite(){
			return true;
		}
		@Override
		public boolean isPositive(){
			return false;
		}
		@Override
		public boolean isNegative(){
			return true;
		}
		@Override
		public double toDouble(){
			return Double.NEGATIVE_INFINITY;
		}
	}
}
