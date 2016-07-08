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
package com.github.chungkwong.jschememin.primitive;
import com.github.chungkwong.jschememin.*;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class DefineLibrary extends PrimitiveType{
	public static final DefineLibrary INSTANCE=new DefineLibrary();
	private static final ScmSymbol EXPORT=new ScmSymbol("export");
	private static final ScmSymbol IMPORT=new ScmSymbol("import");
	private static final ScmSymbol BEGIN=new ScmSymbol("begin");
	private static final ScmSymbol INCLUDE=new ScmSymbol("include");
	private static final ScmSymbol INCLUDE_CI=new ScmSymbol("include-ci");
	private static final ScmSymbol INCLUDE_LIBRARY=new ScmSymbol("include-library-declaration");
	private static final ScmSymbol COND_EXPAND=new ScmSymbol("cond-expand");
	private DefineLibrary(){
		super(new ScmSymbol("define-library"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject expr){
		if(pointer==null){
			ScmPair name=(ScmPair)((ScmPair)expr).getCar();
			expr=((ScmPair)expr).getCdr();
			Environment libEnv=new Environment(env);
			Library lib=new Library(name,new HashMap<ScmSymbol,ScmSymbol>(),env);
			call(env,cont,pointer,expr);
			while(expr instanceof ScmPair){
				ScmPair declaration=(ScmPair)((ScmPair)expr).getCar();
				//ScmSymbol dir=declaration.getCar();

				expr=((ScmPair)expr).getCdr();
			}
		}else{

		}
	}
}