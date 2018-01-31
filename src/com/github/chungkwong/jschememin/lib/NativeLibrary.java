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
import com.github.chungkwong.jschememin.type.*;
import java.io.*;
import java.util.*;
import java.util.logging.*;
/**
 * Loader for native library
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class NativeLibrary implements LibraryLoader{
	private Library lib=null;
	private final ScmPair name;
	/**
	 *
	 * @param part the name of the library
	 */
	public NativeLibrary(String... part){
		this.name=(ScmPair)Arrays.stream(part).map((n)->new ScmSymbol(n)).collect(ScmList.COLLECTOR);
	}
	@Override
	public ScmPair getName(){
		return name;
	}
	@Override
	public Library getLibrary(){
		if(lib==null){
			lib=new Library(name,new HashMap<>(),new SchemeEnvironment(false));
			init(lib);
		}
		return lib;
	}
	/**
	 * Add a object into the library
	 * @param name the variable name
	 * @param obj the object
	 * @param export if the object prefered to be exposed to users
	 */
	protected void addNative(ScmSymbol name,ScmObject obj,boolean export){
		lib.getInternalEnvironment().add(name,obj);
		if(export)
			lib.getExportMap().put(name,name);
	}
	/**
	 * Add a object into the library
	 * @param type
	 */
	protected void addPrimitiveType(BasicConstruct type){
		lib.getInternalEnvironment().addPrimitiveType(type);
		lib.getExportMap().put(type.getKeyword(),type.getKeyword());
	}
	/**
	 * Exexute a derive file and use add variables that it define to the library
	 * @param file the file name
	 * @param export the variable to be exposed to users
	 */
	protected void addDeriveFile(String file,String... export){
		try{
			Parser parser=new Parser(new Lex(new InputStreamReader(NativeLibrary.class.getResourceAsStream(file),"UTF-8")));
			SchemeEnvironment internal=lib.getInternalEnvironment();
			Evaluator evaluator=new Evaluator(internal);
			parser.getRemainingDatums().forEach((d)->evaluator.eval(d));
			for(String name:export){
				ScmSymbol id=new ScmSymbol(name);
				lib.getExportMap().put(id,id);
			}
		}catch(UnsupportedEncodingException ex){
			Logger.getGlobal().log(Level.SEVERE,null,ex);
		}
	}
	/**
	 * Add a object into the library
	 * @param name the variable name
	 * @param proc the object
	 */
	protected void addNativeProcedure(String name,NativeProcedure proc){
		ScmSymbol sym=new ScmSymbol(name);
		lib.getInternalEnvironment().add(sym,new NativeEvaluable(proc));
		lib.getExportMap().put(sym,sym);
	}
	/**
	 * Add a object into the library
	 * @param name the variable name
	 * @param proc the object
	 */
	protected void addNativeProcedureMulti(String name,NativeProcedure proc){
		ScmSymbol sym=new ScmSymbol(name);
		lib.getInternalEnvironment().add(sym,new NativeEvaluableMulti(proc));
		lib.getExportMap().put(sym,sym);
	}
	/**
	 * To be executed when the library actual being loaded
	 * @param lib the library
	 */
	protected abstract void init(Library lib);
}
