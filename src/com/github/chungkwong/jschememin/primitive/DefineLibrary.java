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
		ScmPair name=(ScmPair)((ScmPair)expr).getCar();
		expr=((ScmPair)expr).getCdr();
		Library lib=new Library(name,new HashMap<>(),new Environment(env));
		while(expr instanceof ScmPair){
			ScmPair declaration=(ScmPair)((ScmPair)expr).getCar();
			ScmObject dir=declaration.getCar();
			if(dir.equals(IMPORT)){
				Import.INSTANCE.importLibraries(lib.getInternalEnvironment(),declaration.getCdr());
			}else if(dir.equals(EXPORT)){
				addExportSet(lib,declaration.getCdr());
			}else if(dir.equals(INCLUDE_LIBRARY)){
				ScmPair content=Include.INSTANCE.getFileContent((ScmPair)declaration.getCdr());
				if(content.getCdr()instanceof ScmPair){
					content.getLastListNode().setCdr(((ScmPair)expr).getCdr());
					expr=content;
				}
			}else if(dir.equals(COND_EXPAND)){
				ScmPair content=(ScmPair)((ScmSyntaxRule)env.get(COND_EXPAND)).tranform((ScmPairOrNil)declaration.getCdr());
				if(content.getCdr()instanceof ScmPair){
					content.getLastListNode().setCdr(((ScmPair)expr).getCdr());
					expr=content;
				}
			}else{
				new Evaluator(lib.getInternalEnvironment()).eval(declaration);
			}
			expr=((ScmPair)expr).getCdr();
		}
		cont.ret(lib);
	}
	void addExportSet(Library lib,ScmObject list){
		while(list instanceof ScmPair){
			ScmObject spec=((ScmPair)list).getCar();
			if(spec instanceof ScmSymbol)
				lib.getExportMap().put((ScmSymbol)spec,(ScmSymbol)spec);
			else{
				lib.getExportMap().put((ScmSymbol)((ScmPair)spec).getCaddr(),(ScmSymbol)((ScmPair)spec).getCadr());
			}
			list=((ScmPair)list).getCdr();
		}
	}
}