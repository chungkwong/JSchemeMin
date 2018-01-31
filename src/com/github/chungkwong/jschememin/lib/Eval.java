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
import com.github.chungkwong.jschememin.primitive.*;
/**
 * Correspoding to the library (scheme eval) in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Eval extends NativeLibrary{
	public static final Eval INSTANCE=new Eval();
	private Eval(){
		super("scheme","eval");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("environment",(o)->Import.INSTANCE.importLibraries(new SchemeEnvironment(false),o));
		addPrimitiveType(com.github.chungkwong.jschememin.primitive.Eval.INSTANCE);
		//addNativeProcedure("eval",(o)->new Evaluator((Environment)cadr(o)).eval(car(o)));
	}
}
