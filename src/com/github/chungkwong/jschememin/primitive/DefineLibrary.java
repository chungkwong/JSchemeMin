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
public class DefineLibrary extends BasicConstruct implements Primitive{
	public static final DefineLibrary INSTANCE=new DefineLibrary();
	private static final ScmSymbol EXPORT=new ScmSymbol("export");
	private static final ScmSymbol IMPORT=new ScmSymbol("import");
	private static final ScmSymbol BEGIN=new ScmSymbol("begin");
	private static final ScmSymbol INCLUDE=new ScmSymbol("include");
	private static final ScmSymbol INCLUDE_CI=new ScmSymbol("include-ci");
	private static final ScmSymbol INCLUDE_LIBRARY=new ScmSymbol("include-library-declarations");
	private static final ScmSymbol COND_EXPAND=new ScmSymbol("cond-expand");
	private static final ScmSymbol AND=new ScmSymbol("and");
	private static final ScmSymbol OR=new ScmSymbol("or");
	private static final ScmSymbol NOT=new ScmSymbol("not");
	private static final ScmSymbol LIBRARY=new ScmSymbol("library");
	private DefineLibrary(){
		super(new ScmSymbol("define-library"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil expr){
		ScmPair name=(ScmPair)((ScmPair)expr).getCar();
		expr=(ScmPairOrNil)((ScmPair)expr).getCdr();
		Library lib=new Library(name,new HashMap<>(),new Environment(env));
		while(expr instanceof ScmPair){
			ScmPair declaration=(ScmPair)((ScmPair)expr).getCar();
			ScmObject dir=declaration.getCar();
			if(dir.equals(IMPORT)){
				Import.INSTANCE.importLibraries(lib.getInternalEnvironment(),declaration.getCdr());
			}else if(dir.equals(EXPORT)){
				addExportSet(lib,declaration.getCdr());
			}else if(dir.equals(INCLUDE)){
				ScmPair content=(ScmPair)Include.INSTANCE.getFileContent((ScmPair)declaration.getCdr());
				if(content.getCdr()instanceof ScmPair){
					expr=new ScmPair(content,((ScmPair)expr).getCdr());
					continue;
				}
			}else if(dir.equals(INCLUDE_CI)){
				ScmPair content=(ScmPair)Include.INSTANCE_CI.getFileContent((ScmPair)declaration.getCdr());
				if(content.getCdr()instanceof ScmPair){
					expr=new ScmPair(content,((ScmPair)expr).getCdr());
					continue;
				}
			}else if(dir.equals(INCLUDE_LIBRARY)){
				ScmPair content=(ScmPair)Include.INSTANCE.getFileContent((ScmPair)declaration.getCdr());
				if(content.getCdr()instanceof ScmPair){
					ScmList.getLastListNode(content).setCdr(((ScmPair)expr).getCdr());
					expr=content;
				}
			}else if(dir.equals(COND_EXPAND)){ //FIXME
				ScmPair content=(ScmPair)((ScmSyntaxRules)env.get(COND_EXPAND)).transform((ScmPairOrNil)declaration.getCdr(),env);

				if(content.getCdr() instanceof ScmPair){
					ScmList.getLastListNode(content).setCdr(((ScmPair)expr).getCdr());
					expr=content;
				}
			}else if(dir.equals(BEGIN)){
				ScmList.forEach(declaration.getCdr(),(e)->new Evaluator(lib.getInternalEnvironment()).eval(e));
			}else{
				throw new SyntaxException("Unknown clause: "+dir);
			}
			expr=(ScmPairOrNil)((ScmPair)expr).getCdr();
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
	ScmObject condExpand(ScmPair clauses){
		ScmPair firstClause=(ScmPair)clauses.getCar();
		ScmPair then=(ScmPair)clauses.getCdar();
		ScmPair remainingClauses=(ScmPair)clauses.getCdr();
		ScmObject requirement=firstClause.getCar();
		if(requirement instanceof ScmPair){
			ScmSymbol key=(ScmSymbol)((ScmPair)requirement).getCar();
			if(key.equals(AND)){
				return condExpand(clauses);
			}else if(key.equals(OR)){

			}else if(key.equals(NOT)){
				return condExpand(new ScmPair(((ScmPair)requirement).getCadr(),requirement));
			}else{
				return LibraryManager.hasLibrary((ScmPair)((ScmPair)requirement).getCadr())?then:condExpand(remainingClauses);
			}
		}else{
			return Feature.contains(((ScmSymbol)requirement).getValue())?then:condExpand(remainingClauses);
		}
	}
}