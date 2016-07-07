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
public class Import extends PrimitiveType{
	public static final Import INSTANCE=new Import();
	private static final ScmSymbol PREFIX=new ScmSymbol("prefix");
	private static final ScmSymbol RENAME=new ScmSymbol("rename");
	private static final ScmSymbol EXCEPT=new ScmSymbol("except");
	private static final ScmSymbol ONLY=new ScmSymbol("only");
	private Import(){
		super(new ScmSymbol("import"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject list){
		while(list instanceof ScmPair){
			importLibrary(env,(ScmPair)((ScmPair)list).getCar());
			list=((ScmPair)list).getCdr();
		}
		cont.ret(new ScmSymbol("ok"));
	}
	public void importLibrary(Environment env,ScmPair spec){
		ImportSet set=getImportSet(spec);
		LibraryLoader.getLibrary(set.libName).exportTo(env,set.ex2in);
	}
	public ImportSet getImportSet(ScmPair list){
		if(list.getCdr() instanceof ScmPair&&((ScmPair)list.getCdr()).getCar() instanceof ScmPair){
			ScmObject car=list.getCar();
			ImportSet set=getImportSet((ScmPair)((ScmPair)list.getCdr()).getCar());
			if(car.equals(PREFIX)){

			}else if(car.equals(RENAME)){

			}else if(car.equals(EXCEPT)){

			}else if(car.equals(ONLY)){

			}
		}else
			return new ImportSet(list);
	}
	static class ImportSet{
		final ScmPair libName;
		final HashMap<ScmSymbol,ScmSymbol> ex2in=new HashMap<>();
		public ImportSet(ScmPair libName){
			this.libName=libName;
		}
	}
}
