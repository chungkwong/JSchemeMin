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
public class SimpleLibrary implements LibraryLoader{
	private final ScmPair name;
	private final String path;
	private Library lib=null;
	public SimpleLibrary(String path,String... part){
		ScmListBuilder buf=new ScmListBuilder();
		Arrays.stream(part).map((n)->new ScmSymbol(n)).forEach((o)->buf.add(o));
		this.name=(ScmPair)buf.toList();
		this.path=path;
	}
	@Override
	public Library getLibrary(){
		if(lib==null){
			try{
				new Evaluator(false).eval(new Parser(new Lex(new InputStreamReader(SimpleLibrary.class.getResourceAsStream(path),"UTF-8"))).nextDatum());
			}catch(UnsupportedEncodingException ex){
				Logger.getGlobal().log(Level.SEVERE,null,ex);
			}
			lib=LibraryManager.getLibrary(name);
		}
		return lib;
	}
	@Override
	public ScmPair getName(){
		return name;
	}
}
