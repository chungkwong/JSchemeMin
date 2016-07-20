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
	public static class PositiveNaN extends ScmSpecialReal{
		@Override
		public String toExternalRepresentation(){
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
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmReal sin(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmReal cos(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmReal sqrt(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public int signum(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmReal toExact(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmFloatingPointNumber toInExact(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmReal exp(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
		@Override
		public ScmReal log(){
			throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
		}
	}
	public static class PositiveInf extends ScmSpecialReal{
		@Override
		public String toExternalRepresentation(){
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
	}
	public static class NegativeInf extends ScmSpecialReal{
		@Override
		public String toExternalRepresentation(){
			return "-inf.0";
		}
		@Override
		public ScmReal negate(){
			return POSITIVE_INF;
		}
	}
}
