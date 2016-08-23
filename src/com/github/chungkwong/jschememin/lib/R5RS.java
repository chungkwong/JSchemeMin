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
import java.util.logging.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class R5RS implements LibraryLoader{
	public static final R5RS INSTANCE=new R5RS();
	private static final ScmPair NAME=(ScmPair)ScmList.toList(new ScmSymbol("scheme"),new ScmSymbol("r5rs"));
	private static final String PATH="/com/github/chungkwong/jschememin/lib/r5rs.scm";
	private Library lib=null;
	private R5RS(){

	}
	@Override
	public Library getLibrary(){
		if(lib==null){
			try{
				new Evaluator(false).eval(new Parser(new Lex(new InputStreamReader(R5RS.class.getResourceAsStream(PATH),"UTF-8"))).nextDatum());
			}catch(UnsupportedEncodingException ex){
				Logger.getGlobal().log(Level.SEVERE,null,ex);
			}
			lib=LibraryManager.getLibrary(NAME);
		}
		return lib;
	}
	@Override
	public ScmPair getName(){
		return NAME;
	}
}