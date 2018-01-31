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
import static com.github.chungkwong.jschememin.lib.Utility.car;
import com.github.chungkwong.jschememin.type.*;
/**
 * Correspoding to the library (scheme char) in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Char extends NativeLibrary{
	public static final Char INSTANCE=new Char();
	private Char(){
		super("scheme","char");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("char-alphabetic?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isAlphabetic()));
		addNativeProcedure("char-numeric?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isNumeric()));
		addNativeProcedure("char-whitespace?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isWhiteSpace()));
		addNativeProcedure("char-upper-case?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isUpperCase()));
		addNativeProcedure("char-lower-case?",(o)->ScmBoolean.valueOf(((ScmCharacter)car(o)).isLowerCase()));
		addNativeProcedure("digit-value",(o)->{int d=((ScmCharacter)car(o)).getDigitValue();return d>=0?new ScmInteger(d):ScmBoolean.FALSE;});
		addNativeProcedure("char-upcase",(o)->((ScmCharacter)car(o)).upCase());
		addNativeProcedure("char-downcase",(o)->((ScmCharacter)car(o)).downCase());
		addNativeProcedure("char-foldcase",(o)->((ScmCharacter)car(o)).foldCase());
		addNativeProcedure("string-upcase",(o)->((ScmString)car(o)).toUpperCase());
		addNativeProcedure("string-downcase",(o)->((ScmString)car(o)).toLowerCase());
		addNativeProcedure("string-foldcase",(o)->((ScmString)car(o)).toFoldingCase());
		addNativeProcedure("char-ci=?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareToIgnoreCase((ScmCharacter)b)==0));
		addNativeProcedure("char-ci<?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareToIgnoreCase((ScmCharacter)b)<0));
		addNativeProcedure("char-ci<=?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareToIgnoreCase((ScmCharacter)b)<=0));
		addNativeProcedure("char-ci>?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareToIgnoreCase((ScmCharacter)b)>0));
		addNativeProcedure("char-ci>=?",Utility.chainComparator((a,b)->((ScmCharacter)a).compareToIgnoreCase((ScmCharacter)b)>=0));
		addNativeProcedure("string-ci=?",Utility.chainComparator((a,b)->((ScmString)a).compareToIgnoreCase((ScmString)b)==0));
		addNativeProcedure("string-ci<?",Utility.chainComparator((a,b)->((ScmString)a).compareToIgnoreCase((ScmString)b)<0));
		addNativeProcedure("string-ci<=?",Utility.chainComparator((a,b)->((ScmString)a).compareToIgnoreCase((ScmString)b)<=0));
		addNativeProcedure("string-ci>?",Utility.chainComparator((a,b)->((ScmString)a).compareToIgnoreCase((ScmString)b)>0));
		addNativeProcedure("string-ci>=?",Utility.chainComparator((a,b)->((ScmString)a).compareToIgnoreCase((ScmString)b)>=0));
	}
}
