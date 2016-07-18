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
import com.github.chungkwong.jschememin.primitive.*;
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Base extends NativeLibrary{
	public static final Base INSTANCE=new Base();
	public Base(){
		super((ScmPair)ScmList.toList(new ScmString("scheme"),new ScmString("base")));
	}
	@Override
	protected void init(Library lib){
		addPrimitiveType(If.INSTANCE);
		addPrimitiveType(Assignment.INSTANCE);
		addPrimitiveType(Lambda.INSTANCE);
		addPrimitiveType(Include.INSTANCE);
		addPrimitiveType(Include.INSTANCE_CI);
		addPrimitiveType(Quote.INSTANCE);
		addPrimitiveType(Define.INSTANCE);
		addPrimitiveType(DefineRecordType.INSTANCE);
		addPrimitiveType(DefineLibrary.INSTANCE);
		addPrimitiveType(Import.INSTANCE);
		addPrimitiveType(SyntaxRule.INSTANCE);

		addNativeProcedure("equal?",(o)->ScmBoolean.valueOf(car(o).equals(cadr(o))));
		addNativeProcedure("eqv?",(o)->ScmBoolean.valueOf(car(o).equalsValue(cadr(o))));
		addNativeProcedure("eq?",(o)->ScmBoolean.valueOf(car(o).equalsStrict(cadr(o))));

		addNativeProcedure("number?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("complex?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("real?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("rational?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("integer?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("exact?",(o)->ScmBoolean.valueOf(((ScmNumber)car(o)).isExact()));
		addNativeProcedure("inexact?",(o)->ScmBoolean.valueOf(!((ScmNumber)car(o)).isExact()));

		addNativeProcedure("cons",(o)->new ScmPair(car(o),cadr(o)));
		addNativeProcedure("car",(o)->((ScmPair)car(o)).getCar());
		addNativeProcedure("cdr",(o)->((ScmPair)car(o)).getCdr());
		addNativeProcedure("cadr",(o)->((ScmPair)car(o)).getCadr());
		addNativeProcedure("cdar",(o)->((ScmPair)car(o)).getCdar());

		addNativeProcedure("binary-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryInputPort
				||car(o) instanceof ScmBinaryOutputPort));
		addNativeProcedure("boolean?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBoolean));
		addNativeProcedure("null?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNil));
		addNativeProcedure("pair?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmPair));
		addNativeProcedure("symbol?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmSymbol));
		addNativeProcedure("vector?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmVector));

		addDeriveFile("/com/github/chungkwong/jschememin/lib/base_derive.scm");
	}
	public static void main(String[] args){
		INSTANCE.getLibrary();
	}
}