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
import static com.github.chungkwong.jschememin.lib.Utility.cdr;
import com.github.chungkwong.jschememin.primitive.*;
import com.github.chungkwong.jschememin.type.*;
import java.io.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Base extends NativeLibrary{
	public static final Base INSTANCE=new Base();
	public Base(){
		super("scheme","base");
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
		addNativeProcedure("make-bytevector",new NativeProcedureDefault(
				(o)->ScmByteVector.fill(((ScmComplex)cadr(o)).toScmInteger(),((ScmComplex)car(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO
		));
		addNativeProcedure("bytevector",(o)->ScmByteVector.toByteVector((ScmPairOrNil)o));
		addNativeProcedure("bytevector-length",(o)->new ScmInteger(((ScmByteVector)car(o)).getLength()));
		addNativeProcedure("bytevector-u8-ref",(o)->((ScmByteVector)car(o)).get(((ScmComplex)cadr(o)).intValueExact()));
		addNativeProcedure("bytevector-u8-set!",(o)->((ScmByteVector)car(o)).set(((ScmComplex)cadr(o)).intValueExact(),(ScmComplex)caddr(o)));
		addNativeProcedure("bytevector-append",(o)->ScmByteVector.append((ScmPairOrNil)o));
		addNativeProcedure("utf8->string",new NativeProcedureDefault(
				(o)->((ScmByteVector)car(o)).decodeUTF8(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmByteVector)car(o)).getLength())
		));
		addNativeProcedure("string->utf8",new NativeProcedureDefault(
				(o)->((ScmString)car(o)).toScmByteVector(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmString)car(o)).length())
		));
		addNativeProcedure("bytevector-copy",new NativeProcedureDefault(
				(o)->((ScmByteVector)car(o)).copy(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmByteVector)car(o)).getLength())
		));
		addNativeProcedure("bytevector-copy!",new NativeProcedureDefault(
				(o)->((ScmByteVector)caddr(o)).copyTo((ScmByteVector)car(o),((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact(),((ScmComplex)caddddr(o)).intValueExact()),
				(o)->car(o),
				(o)->cadr(o),
				(o)->caddr(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmByteVector)caddr(o)).getLength())
		));
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
		addNativeProcedure("vector->string",new NativeProcedureDefault(
				(o)->((ScmVector)car(o)).toScmString(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmVector)car(o)).getLength())
		));
		addNativeProcedure("string->vector",new NativeProcedureDefault(
				(o)->((ScmString)car(o)).toScmVector(((ScmComplex)cadr(o)).intValueExact(),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmInteger.ZERO,
				(o)->new ScmInteger(((ScmString)car(o)).length())
		));
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
		addNativeProcedure("list->string",(o)->ScmString.toScmString((ScmPairOrNil)car(o)));
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
		addNativeProcedure("string=?",Utility.chainComparator((a,b)->((ScmString)a).compareTo((ScmString)b)==0));
		addNativeProcedure("string<?",Utility.chainComparator((a,b)->((ScmString)a).compareTo((ScmString)b)<0));
		addNativeProcedure("string<=?",Utility.chainComparator((a,b)->((ScmString)a).compareTo((ScmString)b)<=0));
		addNativeProcedure("string>?",Utility.chainComparator((a,b)->((ScmString)a).compareTo((ScmString)b)>0));
		addNativeProcedure("string>=?",Utility.chainComparator((a,b)->((ScmString)a).compareTo((ScmString)b)>=0));
	}
	private void initCharacter(){
		addNativeProcedure("char?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmCharacter));
		addNativeProcedure("char=?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareTo((ScmCharacter)b)==0));
		addNativeProcedure("char<?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareTo((ScmCharacter)b)<0));
		addNativeProcedure("char<=?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareTo((ScmCharacter)b)<=0));
		addNativeProcedure("char>?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareTo((ScmCharacter)b)>0));
		addNativeProcedure("char>=?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareTo((ScmCharacter)b)>=0));
		addNativeProcedure("char->integer",(o)->new ScmInteger(((ScmCharacter)car(o)).getCodePoint()));
		addNativeProcedure("integer->char",(o)->new ScmCharacter(((ScmComplex)car(o)).toScmInteger().getValue().intValueExact()));

	}
	private void initSymbol(){
		addNativeProcedure("symbol?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmSymbol));
		addNativeProcedure("symbol->string",(o)->new ScmString(((ScmSymbol)car(o)).getValue()));
		addNativeProcedure("string->symbol",(o)->new ScmSymbol(((ScmString)car(o)).getValue()));
		addNativeProcedure("symbol=?",Utility.chainComparator((a,b)->((ScmSymbol)a).equals((ScmSymbol)b)));
	}
	private void initList(){
		addNativeProcedure("cons",(o)->new ScmPair(car(o),cadr(o)));
		addNativeProcedure("car",(o)->((ScmPair)car(o)).getCar());
		addNativeProcedure("cdr",(o)->((ScmPair)car(o)).getCdr());
		addNativeProcedure("caar",(o)->((ScmPair)car(o)).getCaar());
		addNativeProcedure("cadr",(o)->((ScmPair)car(o)).getCadr());
		addNativeProcedure("cdar",(o)->((ScmPair)car(o)).getCdar());
		addNativeProcedure("cddr",(o)->((ScmPair)car(o)).getCddr());
		addNativeProcedure("set-car!",(o)->{((ScmPair)car(o)).setCar(cadr(o));return car(o);});
		addNativeProcedure("set-cdr!",(o)->{((ScmPair)car(o)).setCdr(cadr(o));return car(o);});
		addNativeProcedure("null?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNil));
		addNativeProcedure("pair?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmPair));
		addNativeProcedure("list?",(o)->ScmBoolean.valueOf(ScmList.isList(car(o))));
		addNativeProcedure("make-list",new NativeProcedureDefault(
				(o)->ScmList.fill(cadr(o),((ScmComplex)car(o)).intValueExact()),
				(o)->car(o),
				(o)->ScmNil.NIL
		));
		addNativeProcedure("list",(o)->ScmList.copy((ScmPairOrNil)o));
		addNativeProcedure("length",(o)->new ScmInteger(ScmList.getLength((ScmPairOrNil)car(o))));
		addNativeProcedure("append",(o)->ScmList.append((ScmPairOrNil)o));
		addNativeProcedure("reverse",(o)->ScmList.reverse((ScmPairOrNil)car(o)));
		addNativeProcedure("list-tail",(o)->ScmList.tail((ScmPairOrNil)car(o),((ScmComplex)cadr(o)).intValueExact()));
		addNativeProcedure("list-ref",(o)->ScmList.get((ScmPairOrNil)car(o),((ScmComplex)cadr(o)).intValueExact()));
		addNativeProcedure("list-set!",(o)->{ScmList.set((ScmPair)car(o),((ScmComplex)cadr(o)).intValueExact(),caddr(o));return car(o);});
		addNativeProcedure("list-copy",(o)->ScmList.copy((ScmPairOrNil)car(o)));
	}
	private void initBoolean(){
		addNativeProcedure("boolean?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBoolean));
		addNativeProcedure("not",(o)->ScmBoolean.valueOf(car(o)==ScmBoolean.FALSE));
		addNativeProcedure("boolean=?",Utility.chainComparator((a,b)->(ScmBoolean)a==(ScmBoolean)b));
	}
	private void initNumber(){
		addNativeProcedure("number?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmNumber));
		addNativeProcedure("complex?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex));
		addNativeProcedure("real?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex&&((ScmComplex)car(o)).isReal()));
		addNativeProcedure("rational?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex&&((ScmComplex)car(o)).isRational()));
		addNativeProcedure("integer?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmComplex&&((ScmComplex)car(o)).isInteger()));
		addNativeProcedure("exact?",(o)->ScmBoolean.valueOf(((ScmNumber)car(o)).isExact()));
		addNativeProcedure("inexact?",(o)->ScmBoolean.valueOf(!((ScmNumber)car(o)).isExact()));
		addNativeProcedure("exact-integer?",(o)->ScmBoolean.valueOf(((ScmNumber)car(o)).isExact()&&((ScmComplex)car(o)).isInteger()));
		addNativeProcedure("<",Utility.chainComparator((a,b)->ScmReal.less(((ScmComplex)a).toScmReal(),((ScmComplex)b).toScmReal())));
		addNativeProcedure("<=",Utility.chainComparator((a,b)->ScmReal.lessEquals(((ScmComplex)a).toScmReal(),((ScmComplex)b).toScmReal())));
		addNativeProcedure(">",Utility.chainComparator((a,b)->ScmReal.greater(((ScmComplex)a).toScmReal(),((ScmComplex)b).toScmReal())));
		addNativeProcedure(">=",Utility.chainComparator((a,b)->ScmReal.greaterEquals(((ScmComplex)a).toScmReal(),((ScmComplex)b).toScmReal())));
		addNativeProcedure("=",Utility.chainComparator((a,b)->ScmReal.equals(((ScmComplex)a).getReal(),((ScmComplex)b).getReal())&&ScmReal.equals(((ScmComplex)a).getImag(),((ScmComplex)b).getImag())));
		addNativeProcedure("zero?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).isZero()));
		addNativeProcedure("positive?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmReal().isPositive()));
		addNativeProcedure("negative?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmReal().isNegative()));
		addNativeProcedure("even?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmInteger().isEven()));
		addNativeProcedure("odd?",(o)->ScmBoolean.valueOf(((ScmComplex)car(o)).toScmInteger().isOdd()));
		addNativeProcedure("+",Utility.reducer((a,b)->((ScmComplex)a).add((ScmComplex)b),ScmInteger.ZERO));
		addNativeProcedure("*",Utility.reducer((a,b)->((ScmComplex)a).multiply((ScmComplex)b),ScmInteger.ONE));
		addNativeProcedure("-",Utility.reducer((a,b)->((ScmComplex)a).subtract((ScmComplex)b),(a)->((ScmComplex)a).negate()));
		addNativeProcedure("/",Utility.reducer((a,b)->((ScmComplex)a).divide((ScmComplex)b),(a)->ScmInteger.ONE.divide((ScmComplex)a)));
		addNativeProcedure("max",Utility.reducer((a,b)->ScmReal.max(((ScmComplex)a).toScmReal(),((ScmComplex)b).toScmReal())));
		addNativeProcedure("min",Utility.reducer((a,b)->ScmReal.min(((ScmComplex)a).toScmReal(),((ScmComplex)b).toScmReal())));
		addNativeProcedure("abs",(o)->((ScmComplex)car(o)).toScmReal().getMagnitude());
		addNativeProcedureMulti("floor/",Utility.correctExactnessMulti((o)->ScmList.toList(((ScmComplex)car(o)).toScmInteger().quotientAndRemainder(((ScmComplex)cadr(o)).toScmInteger()))));
		addNativeProcedure("floor-quotient",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().moduloQuotient(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("floor-remainder",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().moduloRemainder(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedureMulti("truncate/",Utility.correctExactnessMulti((o)->ScmList.toList(((ScmComplex)car(o)).toScmInteger().divideAndRemainder(((ScmComplex)cadr(o)).toScmInteger()))));
		addNativeProcedure("truncate-quotient",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().divide(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("truncate-remainder",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().remainder(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("quotient",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().divide(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("remainder",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().remainder(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("modulo",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmInteger().moduloRemainder(((ScmComplex)cadr(o)).toScmInteger())));
		addNativeProcedure("gcd",Utility.correctExactness(Utility.reducer((a,b)->((ScmComplex)a).toScmInteger().gcd(((ScmComplex)b).toScmInteger()),ScmInteger.ZERO)));
		addNativeProcedure("lcm",Utility.correctExactness(Utility.reducer((a,b)->((ScmComplex)a).toScmInteger().lcm(((ScmComplex)b).toScmInteger()),ScmInteger.ONE)));
		addNativeProcedure("numerator",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmRational().getNumerator()));
		addNativeProcedure("denominator",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmRational().getDenominator()));
		addNativeProcedure("rationalize",(o)->ScmRational.rationalize(((ScmComplex)car(o)).toScmReal(),((ScmComplex)cadr(o)).toScmReal()));
		addNativeProcedure("floor",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmReal().floor()));
		addNativeProcedure("ceiling",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmReal().ceiling()));
		addNativeProcedure("round",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmReal().round()));
		addNativeProcedure("truncate",Utility.correctExactness((o)->((ScmComplex)car(o)).toScmReal().truncate()));
		addNativeProcedure("square",(o)->((ScmComplex)car(o)).square());
		addNativeProcedureMulti("exact-integer-sqrt",Utility.correctExactnessMulti((o)->ScmList.toList(((ScmComplex)car(o)).toScmInteger().sqrtExact())));
		addNativeProcedure("expt",(o)->((ScmComplex)car(o)).pow((ScmComplex)cadr(o)));
		addNativeProcedure("exact",(o)->((ScmComplex)car(o)).toExact());
		addNativeProcedure("inexact",(o)->((ScmComplex)car(o)).toInExact());
		addNativeProcedure("number->string",new NativeProcedureDefault((o)->new ScmString(((ScmComplex)car(o)).toExternalRepresentation(((ScmComplex)cadr(o)).intValueExact())),
				(o)->car(o),(o)->new ScmInteger(10)));
		addNativeProcedure("string->number",new NativeProcedureDefault((o)->(ScmComplex)new Lex(Utility.toRadixPrefix(((ScmComplex)cadr(o)).intValueExact())+((ScmString)car(o)).getValue()).nextToken(),
				(o)->car(o),(o)->new ScmInteger(10)));
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
		//addPrimitiveType(Import.INSTANCE);
		addPrimitiveType(SyntaxRule.INSTANCE);
		addPrimitiveType(Apply.INSTANCE);
		addPrimitiveType(CallWithCurrentContinuation.INSTANCE);
		addPrimitiveType(WithExceptionHandler.INSTANCE);
		addPrimitiveType(Quasiquote.INSTANCE);
		addPrimitiveType(RaiseContinuable.INSTANCE);
		addPrimitiveType(CallWithValues.INSTANCE);
	}
	private void initControl(){
		addNativeProcedure("procedure?",(o)->ScmBoolean.valueOf(car(o) instanceof Evaluable&&!(car(o) instanceof ScmSyntaxRules)&&!(car(o) instanceof Primitive)));
		addPrimitiveType(Apply.INSTANCE);



	}
	private void initException(){
		addNativeProcedure("raise",(o)->{throw ScmError.toException(car(o));});
		addNativeProcedure("error",(o)->{throw ScmError.toException(new ScmError((ScmString)car(o),(ScmPairOrNil)cdr(o),ScmError.ErrorType.OTHER));});
		addNativeProcedure("syntax-error",(o)->{throw ScmError.toException(new ScmError((ScmString)car(o),(ScmPairOrNil)cdr(o),ScmError.ErrorType.SYNTAX));});
		addNativeProcedure("error-object?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmError));
		addNativeProcedure("file-error?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmError&&((ScmError)car(o)).getType()==ScmError.ErrorType.FILE));
		addNativeProcedure("read-error?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmError&&((ScmError)car(o)).getType()==ScmError.ErrorType.READ));
		addNativeProcedure("error-object-message",(o)->((ScmError)car(o)).getErrorMessage());
		addNativeProcedure("error-object-irritants",(o)->((ScmError)car(o)).getIrritants());

	}
	private void initEval(){

	}
	private void initIO(){
		addNativeProcedure("set-current-output-port!",(o)->{ScmPort.CURRENT_OUTPUT=(ScmPort)car(o);return car(o);});
		addNativeProcedure("set-current-input-port!",(o)->{ScmPort.CURRENT_INPUT=(ScmPort)car(o);return car(o);});
		addNativeProcedure("eof-object",(o)->ScmEndOfFileObject.INSTANCE);
		addNativeProcedure("eof-object?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmEndOfFileObject));
		addNativeProcedure("port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmPort));
		addNativeProcedure("binary-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryInputPort
				||car(o) instanceof ScmBinaryOutputPort));
		addNativeProcedure("textual-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmTextualInputPort
				||car(o) instanceof ScmTextualOutputPort));
		addNativeProcedure("input-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryInputPort
				||car(o) instanceof ScmTextualInputPort));
		addNativeProcedure("output-port?",(o)->ScmBoolean.valueOf(car(o) instanceof ScmBinaryOutputPort
				||car(o) instanceof ScmTextualOutputPort));
		addNativeProcedure("input-port-open?",(o)->ScmBoolean.valueOf(!((ScmPort)car(o)).isClosed()));
		addNativeProcedure("output-port-open?",(o)->ScmBoolean.valueOf(!((ScmPort)car(o)).isClosed()));
		addNativeProcedure("current-input-port",(o)->ScmPort.CURRENT_INPUT);
		addNativeProcedure("current-output-port",(o)->ScmPort.CURRENT_OUTPUT);
		addNativeProcedure("current-error-port",(o)->ScmPort.CURRENT_ERROR);
		addNativeProcedure("open-input-file",(o)->new ScmTextualInputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("open-binary-input-file",(o)->new ScmBinaryInputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("open-output-file",(o)->new ScmTextualOutputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("open-binary-output-file",(o)->new ScmBinaryOutputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("close-port",(o)->((ScmPort)car(o)).close());
		addNativeProcedure("close-input-port",(o)->((ScmPort)car(o)).close());
		addNativeProcedure("close-output-port",(o)->((ScmPort)car(o)).close());
		addNativeProcedure("open-input-string",(o)->new ScmTextualInputPort(new StringReader(((ScmString)car(o)).getValue())));
		addNativeProcedure("open-output-string",(o)->new ScmTextualOutputPort(new StringWriter()));
		addNativeProcedure("get-output-string",(o)->new ScmString(((ScmTextualOutputPort)car(o)).getString()));
		addNativeProcedure("open-input-bytevector",(o)->new ScmBinaryInputPort(new ByteArrayInputStream(((ScmByteVector)car(o)).getByteArray())));
		addNativeProcedure("open-output-bytevector",(o)->new ScmBinaryOutputPort(new ByteArrayOutputStream()));
		addNativeProcedure("get-output-bytevector",(o)->new ScmByteVector(((ScmBinaryOutputPort)car(o)).getByteArray()));
		addNativeProcedure("read-char",new NativeProcedureDefault((o)->((ScmTextualInputPort)car(o)).readCharacter(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("peek-char",new NativeProcedureDefault((o)->((ScmTextualInputPort)car(o)).peekCharacter(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("read-line",new NativeProcedureDefault((o)->((ScmTextualInputPort)car(o)).readLine(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("char-ready?",new NativeProcedureDefault((o)->((ScmTextualInputPort)car(o)).ready(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("read-string",new NativeProcedureDefault((o)->((ScmTextualInputPort)cadr(o)).readString(((ScmComplex)car(o)).toScmInteger()),
			(o)->car(o),(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("read-u8",new NativeProcedureDefault((o)->((ScmBinaryInputPort)car(o)).readByte(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("peek-u8",new NativeProcedureDefault((o)->((ScmBinaryInputPort)car(o)).peekByte(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("u8-ready?",new NativeProcedureDefault((o)->((ScmBinaryInputPort)car(o)).ready(),
			(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("read-bytevector",new NativeProcedureDefault((o)->((ScmBinaryInputPort)cadr(o)).readBytevector(((ScmComplex)car(o)).toScmInteger()),
			(o)->car(o),(o)->ScmPort.CURRENT_INPUT));
		addNativeProcedure("read-bytevector!",new NativeProcedureDefault((o)->((ScmBinaryInputPort)cadr(o)).readBytevector((ScmByteVector)car(o),((ScmComplex)cadr(o)).toScmInteger(),((ScmComplex)caddr(o)).toScmInteger()),
			(o)->car(o),(o)->ScmPort.CURRENT_INPUT,(o)->ScmInteger.ZERO,(o)->new ScmInteger(((ScmByteVector)car(o)).getLength())));
		addNativeProcedure("newline",new NativeProcedureDefault((o)->((ScmTextualOutputPort)car(o)).newline(),
			(o)->ScmPort.CURRENT_OUTPUT));
		addNativeProcedure("write-char",new NativeProcedureDefault((o)->((ScmTextualOutputPort)cadr(o)).writeCharacter((ScmCharacter)car(o)),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT));
		addNativeProcedure("write-string",new NativeProcedureDefault((o)->((ScmTextualOutputPort)cadr(o)).writeString((ScmString)car(o),((ScmComplex)caddr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact()),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT,(o)->ScmInteger.ZERO,(o)->new ScmInteger(((ScmString)car(o)).length())));
		addNativeProcedure("write-u8",new NativeProcedureDefault((o)->((ScmBinaryOutputPort)cadr(o)).writeByte(((ScmComplex)car(o)).toScmInteger()),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT));
		addNativeProcedure("write-bytevector",new NativeProcedureDefault((o)->((ScmBinaryOutputPort)cadr(o)).writeByteVector((ScmByteVector)car(o),((ScmComplex)caddr(o)).intValueExact(),((ScmComplex)cadddr(o)).intValueExact()),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT,(o)->ScmInteger.ZERO,(o)->new ScmInteger(((ScmByteVector)car(o)).getLength())));
		addNativeProcedure("flush-output-port",new NativeProcedureDefault((o)->{
			if(car(o)instanceof ScmBinaryOutputPort)return ((ScmBinaryOutputPort)car(o)).flush();
			else return ((ScmTextualOutputPort)car(o)).flush();
		},(o)->ScmPort.CURRENT_OUTPUT));
	}
	private void initSystem(){
		addNativeProcedure("features",(o)->Feature.getAll());

	}
}