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
import static com.github.chungkwong.jschememin.lib.Utility.caddddr;
import static com.github.chungkwong.jschememin.lib.Utility.cadddr;
import static com.github.chungkwong.jschememin.lib.Utility.caddr;
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
		addNativeProcedure("make-vector",new NativeProcedureDefault(
				(o)->ScmVector.fill(cadr(o),((ScmComplex)car(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmNil.NIL
		));
		addNativeProcedure("vector",(o)->ScmVector.toVector((ScmPairOrNil)o));
		addNativeProcedure("vector-length",(o)->new ScmInteger(((ScmVector)car(o)).getLength()));
		addNativeProcedure("vector-ref",(o)->((ScmVector)car(o)).get(((ScmComplex)cadr(o)).intValueExact()));
		addNativeProcedure("vector-set!",(o)->((ScmVector)car(o)).set(((ScmComplex)cadr(o)).intValueExact(),caddr(o)));
		addNativeProcedure("vector-append",(o)->ScmVector.append((ScmPairOrNil)o));
		addNativeProcedure("vector->list",new NativeProcedureDefault(
				(o)->((ScmVector)car(o)).toList(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmVector)car(o)).getLength())
		));
		addNativeProcedure("list->vector",(o)->ScmVector.toVector((ScmPairOrNil)o));
		//vector->string string->vector
		addNativeProcedure("vector-copy",new NativeProcedureDefault(
				(o)->((ScmVector)car(o)).copy(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmVector)car(o)).getLength())
		));
		addNativeProcedure("vector-copy!",new NativeProcedureDefault(
				(o)->((ScmVector)caddr(o)).copyTo((ScmVector)car(o),((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact(),((ScmComplex)caddddr(o)).intValueExact()),
				(o)->car(o),
				(o)->cadr(o),
				(o)->caddr(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmVector)caddr(o)).getLength())
		));
		addNativeProcedure("vector-fill!",new NativeProcedureDefault(
				(o)->((ScmVector)car(o)).setRange(cadr(o),((ScmComplex)caddr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact()),
				(o)->car(o),
				(o)->cadr(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmVector)car(o)).getLength())
		));
	}
	private void initString(){
		addNativeProcedure("string?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmString));
		addNativeProcedure("make-string",new NativeProcedureDefault(
				(o)->ScmString.fill((ScmCharacter)cadr(o),((ScmComplex)car(o)).intValueExact()),
				(o)->car(o),
				(o)->new ScmCharacter(' ')
		));
		addNativeProcedure("string",(o)->ScmString.toScmString((ScmPairOrNil)o));
		addNativeProcedure("string-length",(o)->new ScmInteger(((ScmString)car(o)).length()));
		addNativeProcedure("string-ref",(o)->((ScmString)car(o)).get(((ScmComplex)cadr(o)).intValueExact()));
		addNativeProcedure("string-set!",(o)->((ScmString)car(o)).set(((ScmComplex)cadr(o)).intValueExact(),(ScmCharacter)caddr(o)));
		addNativeProcedure("substring",(o)->((ScmString)car(o)).substring(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()));
		addNativeProcedure("string-append",(o)->ScmString.append((ScmPairOrNil)o));
		addNativeProcedure("string->list",new NativeProcedureDefault(
				(o)->((ScmString)car(o)).toList(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmString)car(o)).length())
		));
		addNativeProcedure("list->string",(o)->ScmString.toScmString((ScmPairOrNil)o));
		addNativeProcedure("string-copy",new NativeProcedureDefault(
				(o)->((ScmString)car(o)).copy(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmString)car(o)).length())
		));
		addNativeProcedure("string-copy!",new NativeProcedureDefault(
				(o)->((ScmString)caddr(o)).copyTo((ScmString)car(o),((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact(),((ScmComplex)caddddr(o)).intValueExact()),
				(o)->car(o),
				(o)->cadr(o),
				(o)->caddr(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmString)caddr(o)).length())
		));
		addNativeProcedure("string-fill!",new NativeProcedureDefault(
				(o)->((ScmString)car(o)).setRange((ScmCharacter)cadr(o),((ScmComplex)caddr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact()),
				(o)->car(o),
				(o)->cadr(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmString)car(o)).length())
		));

	}
	private void initCharacter(){
		addNativeProcedure("char?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmCharacter));
		//compare
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
		addNativeProcedure("car",(o)->((ScmPair)car(o)).getCaar());
		addNativeProcedure("cadr",(o)->((ScmPair)car(o)).getCadr());
		addNativeProcedure("cdar",(o)->((ScmPair)car(o)).getCdar());
		addNativeProcedure("cddr",(o)->((ScmPair)car(o)).getCddr());
		addNativeProcedure("set-car!",(o)->{((ScmPair)car(o)).setCar(cadr(o));return car(o);});
		addNativeProcedure("set-cdr!",(o)->{((ScmPair)car(o)).setCdr(cadr(o));return car(o);});
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
		addNativeProcedure("square",(o)->((ScmComplex)car(o)).square());

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

	}
	private void initException(){

	}
	private void initEval(){

	}
	private void initIO(){
		addNativeProcedure("binary-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryInputPort
				||car(o) instanceof ScmBinaryOutputPort));
		addNativeProcedure("textual-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmTextualInputPort
				||car(o) instanceof ScmTextualOutputPort));
		addNativeProcedure("input-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryInputPort
				||car(o) instanceof ScmTextualInputPort));
		addNativeProcedure("output-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryOutputPort
				||car(o) instanceof ScmTextualOutputPort));
	}
	private void initSystem(){
		addNativeProcedure("features",(o)->Feature.getAll());
	}
}