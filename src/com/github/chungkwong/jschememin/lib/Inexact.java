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
import static com.github.chungkwong.jschememin.lib.Utility.cdr;
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Inexact extends NativeLibrary{
	public static final Inexact INSTANCE=new Inexact();
	public Inexact(){
		super("scheme","inexact");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("finite?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isFinite()));
		addNativeProcedure("infinite?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isInfinite()));
		addNativeProcedure("nan?",(o)->ScmBoolean.valueOf(car(o)instanceof ScmSpecialReal.PositiveInf));
		addNativeProcedure("log",(o)->cdr(o)instanceof ScmNil?((ScmComplex)car(o)).log():((ScmComplex)car(o)).log((ScmComplex)cadr(o)));
		addNativeProcedure("exp",(o)->((ScmComplex)car(o)).exp());
		addNativeProcedure("sin",(o)->((ScmComplex)car(o)).sin());
		addNativeProcedure("cos",(o)->((ScmComplex)car(o)).cos());
		addNativeProcedure("tan",(o)->((ScmComplex)car(o)).tan());
		addNativeProcedure("asin",(o)->((ScmComplex)car(o)).arcsin());
		addNativeProcedure("acos",(o)->((ScmComplex)car(o)).arccos());
		addNativeProcedure("atan",(o)->cdr(o)instanceof ScmNil?((ScmComplex)car(o)).arctan():
				new ScmComplexRectangular(((ScmComplex)cadr(o)).toScmReal(),((ScmComplex)car(o)).toScmReal()).getAngle());
		addNativeProcedure("sqrt",(o)->((ScmComplex)car(o)).sqrt());

	}
}
