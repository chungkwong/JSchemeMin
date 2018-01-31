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
package com.github.chungkwong.jschememin.lib;
import com.github.chungkwong.jschememin.*;
import static com.github.chungkwong.jschememin.lib.Utility.cadr;
import static com.github.chungkwong.jschememin.lib.Utility.car;
import com.github.chungkwong.jschememin.type.*;
/**
 * Correspoding to the library (scheme complex) in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Complex extends NativeLibrary{
	public static final Complex INSTANCE=new Complex();
	private Complex(){
		super("scheme","complex");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("imag-part",(o)->((ScmComplex)car(o)).getImag());
		addNativeProcedure("real-part",(o)->((ScmComplex)car(o)).getReal());
		addNativeProcedure("angle",(o)->((ScmComplex)car(o)).getAngle());
		addNativeProcedure("magnitude",(o)->((ScmComplex)car(o)).getMagnitude());
		addNativeProcedure("make-rectangular",(o)->new ScmComplexRectangular(
				((ScmComplex)car(o)).toScmReal(),((ScmComplex)cadr(o)).toScmReal()));
		addNativeProcedure("make-polar",(o)->new ScmComplexPolar(
				((ScmComplex)car(o)).toScmReal(),((ScmComplex)cadr(o)).toScmReal()));
	}
}
