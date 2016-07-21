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
		initPrimary(lib);
		initEquivalience();
		initNumber();
		initBoolean();
		initList();
		initSymbol();
		initCharacter();
		initString();
		initVector();
		initByteVector();
		initControl();
		initException();
		initEval();
		initIO();
		initSystem();
		addDeriveFile("/com/github/chungkwong/jschememin/lib/base_derive.scm");
	}
	private void initByteVector(){
		addNativeProcedure("bytevector?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmByteVector));
	}
	private void initVector(){
		addNativeProcedure("vector?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmVector));
		//addNativeProcedure("make-vector",(o)->ScmVector.ScmBoolean.valueOf(car(o) instanceof ScmVector));
	}
	private void initString(){
		addNativeProcedure("string?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmString));
	}
	private void initCharacter(){
		addNativeProcedure("char?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmCharacter));
		//compare
		addNativeProcedure("char-alphabetic?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isAlphabetic()));
		addNativeProcedure("char-numeric?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isNumeric()));
		addNativeProcedure("char-whitespace?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isWhiteSpace()));
		addNativeProcedure("char-upper-case?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isLowerCase()));
		addNativeProcedure("char-lower-case?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isUpperCase()));
		addNativeProcedure("digit-value",(o)->new ScmInteger(((ScmCharacter)car(o)).getDigitValue()));
		addNativeProcedure("char-upcase",(o)->((ScmCharacter)car(o)).upCase());
		addNativeProcedure("char-downcase",(o)->((ScmCharacter)car(o)).downCase());
		addNativeProcedure("char-foldcase",(o)->((ScmCharacter)car(o)).foldCase());
		addNativeProcedure("char->integer",(o)->new ScmInteger(((ScmCharacter)car(o)).getCodePoint()));
		addNativeProcedure("integer->char",(o)->new ScmCharacter(((ScmComplex)car(o)).toScmInteger().getValue().intValueExact()));
		
	}
	private void initSymbol(){
		addNativeProcedure("symbol?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmSymbol));
		addNativeProcedure("symbol->string",(o)->new ScmString(((ScmSymbol)car(o)).getValue()));
		addNativeProcedure("string->symbol",(o)->new ScmSymbol(((ScmString)car(o)).getValue()));
		//symbol=?
	}
	private void initList(){
		addNativeProcedure("cons",(o)->new ScmPair(car(o),cadr(o)));
		addNativeProcedure("car",(o)->((ScmPair)car(o)).getCar());
		addNativeProcedure("cdr",(o)->((ScmPair)car(o)).getCdr());
		addNativeProcedure("cadr",(o)->((ScmPair)car(o)).getCadr());
		addNativeProcedure("cdar",(o)->((ScmPair)car(o)).getCdar());
		addNativeProcedure("null?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNil));
		addNativeProcedure("pair?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmPair));
	}
	private void initBoolean(){
		addNativeProcedure("boolean?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBoolean));
		addNativeProcedure("not",(o)->ScmBoolean.valueOf(car(o)!=ScmBoolean.FALSE));
		//boolean=?
	}
	private void initNumber(){
		addNativeProcedure("number?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("complex?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex));
		addNativeProcedure("real?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex&&((ScmComplex)car(o)).isReal()));
		addNativeProcedure("rational?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex&&((ScmComplex)car(o)).isRational()));
		addNativeProcedure("integer?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex&&((ScmComplex)car(o)).isInteger()));
		addNativeProcedure("exact?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isExact()));
		addNativeProcedure("inexact?",(o)->ScmBoolean.valueOf(!((ScmNumber)car(o)).isExact()));
		addNativeProcedure("exact-integer?",(o)->ScmBoolean.valueOf(((ScmNumber)car(o)).isExact()&&((ScmComplex)car(o)).isInteger()));
		addNativeProcedure("finite?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isFinite()));
		addNativeProcedure("infinite?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isInfinite()));
		addNativeProcedure("nan?",(o)->ScmBoolean.valueOf(car(o)instanceof ScmSpecialReal.PositiveInf));
		//compare
		addNativeProcedure("zero?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isZero()));
		addNativeProcedure("positive?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmReal().isPositive()));
		addNativeProcedure("negative?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmReal().isNegative()));
		addNativeProcedure("even?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmInteger().isEven()));
		addNativeProcedure("odd?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmInteger().isOdd()));
		//min min + * - /
		addNativeProcedure("abs",(o)->((ScmComplex)car(o)).toScmReal().getMagnitude());
		addNativeProcedure("floor/",(o)->ScmList.toList(((ScmComplex)car(o)).toScmInteger().divideAndRemainder(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("floor-quotient",(o)->((ScmComplex)car(o)).toScmInteger().moduloQuotient(((ScmComplex)cadr(o)).toScmInteger()));
		addNativeProcedure("floor-remainder",(o)->((ScmComplex)car(o)).toScmInteger().moduloRemainder(((ScmComplex)cadr(o)).toScmInteger()));
		addNativeProcedure("truncate/",(o)->ScmList.toList(((ScmComplex)car(o)).toScmInteger().quotientAndRemainder(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("truncate-quotient",(o)->((ScmComplex)car(o)).toScmInteger().divide(((ScmComplex)cadr(o)).toScmInteger()));
		addNativeProcedure("truncate-remainder",(o)->((ScmComplex)car(o)).toScmInteger().remainder(((ScmComplex)cadr(o)).toScmInteger()));
		addNativeProcedure("quotient",(o)->((ScmComplex)car(o)).toScmInteger().divide(((ScmComplex)cadr(o)).toScmInteger()));
		addNativeProcedure("remainder",(o)->((ScmComplex)car(o)).toScmInteger().remainder(((ScmComplex)cadr(o)).toScmInteger()));
		addNativeProcedure("module",(o)->((ScmComplex)car(o)).toScmInteger().moduloRemainder(((ScmComplex)cadr(o)).toScmInteger()));
		//gcd lcm
		addNativeProcedure("numerator",(o)->((ScmComplex)car(o)).toScmRational().getNumerator());
		addNativeProcedure("denominator",(o)->((ScmComplex)car(o)).toScmRational().getDenominator());
		addNativeProcedure("floor",(o)->((ScmComplex)car(o)).toScmReal().floor());
		addNativeProcedure("ceiling",(o)->((ScmComplex)car(o)).toScmReal().ceiling());
		addNativeProcedure("round",(o)->((ScmComplex)car(o)).toScmReal().round());
		addNativeProcedure("truncate",(o)->((ScmComplex)car(o)).toScmReal().truncate());
		addNativeProcedure("log",(o)->cdr(o)instanceof ScmNil?((ScmComplex)car(o)).log():((ScmComplex)car(o)).log((ScmComplex)cadr(o)));
		addNativeProcedure("exp",(o)->((ScmComplex)car(o)).exp());
		addNativeProcedure("sin",(o)->((ScmComplex)car(o)).sin());
		addNativeProcedure("cos",(o)->((ScmComplex)car(o)).cos());
		addNativeProcedure("tan",(o)->((ScmComplex)car(o)).tan());
		addNativeProcedure("asin",(o)->((ScmComplex)car(o)).arcsin());
		addNativeProcedure("acos",(o)->((ScmComplex)car(o)).arccos());
		addNativeProcedure("atan",(o)->cdr(o)instanceof ScmNil?((ScmComplex)car(o)).arctan():
				new ScmComplexRectangular(((ScmComplex)cadr(o)).toScmReal(),((ScmComplex)car(o)).toScmReal()).getAngle());
		addNativeProcedure("square",(o)->((ScmComplex)car(o)).square());
		addNativeProcedure("sqrt",(o)->((ScmComplex)car(o)).sqrt());
		addNativeProcedure("exact-integer-sqrt",(o)->ScmList.toList(((ScmComplex)car(o)).toScmInteger().sqrtExact()));
		addNativeProcedure("expt",(o)->((ScmComplex)car(o)).pow((ScmComplex)cadr(o)));
		addNativeProcedure("exact",(o)->((ScmComplex)car(o)).toExact());
		addNativeProcedure("inexact",(o)->((ScmComplex)car(o)).toInExact());
	}
	private void initEquivalience(){
		addNativeProcedure("equal?",(o)->ScmBoolean.valueOf(car(o).equals(cadr(o))));
		addNativeProcedure("eqv?",(o)->ScmBoolean.valueOf(car(o).equalsValue(cadr(o))));
		addNativeProcedure("eq?",(o)->ScmBoolean.valueOf(car(o).equalsStrict(cadr(o))));
	}
	private void initPrimary(Library lib){
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
	}
	private void initControl(){
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
	private void initException(){
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
	private void initEval(){
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
	private void initIO(){
		addNativeProcedure("binary-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryInputPort
				||car(o) instanceof ScmBinaryOutputPort));
	}
	private void initSystem(){
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
}