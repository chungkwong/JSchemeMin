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
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class NativeLibrary{
	private Library lib=null;
	private final ScmPair name;
	public NativeLibrary(String... part){
		ScmListBuilder buf=new ScmListBuilder();
		Arrays.stream(part).map((n)->new ScmSymbol(n)).forEach((o)->buf.add(o));
		this.name=(ScmPair)buf.toList();
	}
	public ScmPair getName(){
		return name;
	}
	public Library getLibrary(){
		if(lib==null){
			lib=new Library(name,new HashMap<>(),new Environment(false));
			init(lib);
		}
		return lib;
	}
	protected void addNative(ScmSymbol name,ScmObject obj){
		lib.getInternalEnvironment().add(name,obj);
		lib.getExportMap().put(name,name);
	}
	protected void addPrimitiveType(PrimitiveType type){
		lib.getInternalEnvironment().addPrimitiveType(type);
		lib.getExportMap().put(type.getKeyword(),type.getKeyword());
	}
	protected void addDeriveFile(String file){
		try{
			Parser parser=new Parser(new Lex(new InputStreamReader(NativeLibrary.class.getResourceAsStream(file),"UTF-8")));
			Environment internal=lib.getInternalEnvironment();
			Environment tmp=new Environment(internal);
			Evaluator evaluator=new Evaluator(tmp);
			parser.getRemainingDatums().forEach((d)->evaluator.eval(d));
			tmp.getBindings().forEach((id,obj)->{addNative(id,obj);});
		}catch(UnsupportedEncodingException ex){
			Logger.getGlobal().log(Level.SEVERE,null,ex);
		}
	}
	protected void addNativeProcedure(String name,NativeProcedure proc){
		ScmSymbol sym=new ScmSymbol(name);
		lib.getInternalEnvironment().add(sym,new NativeEvaluable(proc));
		lib.getExportMap().put(sym,sym);
	}
	protected abstract void init(Library lib);
}
