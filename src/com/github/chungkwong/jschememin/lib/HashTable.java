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
import static com.github.chungkwong.jschememin.lib.Utility.caddr;
import static com.github.chungkwong.jschememin.lib.Utility.cadr;
import static com.github.chungkwong.jschememin.lib.Utility.car;
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class HashTable extends NativeLibrary{
	private static final ScmSymbol OK=new ScmSymbol("ok");
	public static final HashTable INSTANCE=new HashTable();
	private HashTable(){
		super("scheme","hashtables");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("make-hashtable",new NativeProcedureDefault(
				(o)->new ScmHashTable((Evaluable)car(o),(Evaluable)cadr(o),((ScmComplex)caddr(o)).intValueExact()),
				(o)->car(o),(o)->cadr(o),(o)->new ScmInteger(16)));
		addNativeProcedure("hashtable-copy",new NativeProcedureDefault(
				(o)->((ScmHashTable)car(o)).copy(((ScmBoolean)cadr(o)).isTrue()),
				(o)->car(o),(o)->ScmBoolean.FALSE));
		addNativeProcedure("hashtable-mutable?",(o)->ScmBoolean.valueOf(((ScmHashTable)car(o)).isMutable()));
		addNativeProcedure("hashtable-equivalence-function",(o)->((ScmHashTable)car(o)).getEquivalenceFunction());
		addNativeProcedure("hashtable-hash-function",(o)->((ScmHashTable)car(o)).getHashFunction());
		addNativeProcedure("hashtable?",(o)->ScmBoolean.valueOf(car(o)instanceof ScmHashTable));
		addNativeProcedure("hashtable-contains?",(o)->ScmBoolean.valueOf(((ScmHashTable)car(o)).contains(cadr(o))));
		addNativeProcedure("hashtable-size",(o)->new ScmInteger(((ScmHashTable)car(o)).size()));
		addNativeProcedure("hashtable-ref",(o)->((ScmHashTable)car(o)).get(cadr(o),caddr(o)));
		addNativeProcedure("hashtable-set!",(o)->{((ScmHashTable)car(o)).put(cadr(o),caddr(o));return OK;});
		addNativeProcedure("hashtable-delete!",(o)->{((ScmHashTable)car(o)).remove(cadr(o));return OK;});
		addNativeProcedure("hashtable-clear!",(o)->{((ScmHashTable)car(o)).clear();return OK;});
		addNativeProcedure("hashtable-keys",(o)->((ScmHashTable)car(o)).keys());
		addNativeProcedureMulti("hashtable-entries",(o)->ScmList.toList(((ScmHashTable)car(o)).keys(),((ScmHashTable)car(o)).values()));
		addNativeProcedure("equal-hash",(o)->new ScmInteger(((ScmObject)car(o)).hashCode()));
		addNativeProcedure("string-hash",(o)->new ScmInteger(((ScmString)car(o)).hashCode()));
		addNativeProcedure("string-ci-hash",(o)->new ScmInteger(((ScmString)car(o)).toFoldingCase().hashCode()));
		addNativeProcedure("symbol-hash",(o)->new ScmInteger(((ScmSymbol)car(o)).hashCode()));
		addDeriveFile("/com/github/chungkwong/jschememin/lib/hashtables_derive.scm",
				"make-eq-hashtable","make-eqv-hashtable","hashtable-update!");
	}
}
