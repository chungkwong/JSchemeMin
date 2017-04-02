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
import static com.github.chungkwong.jschememin.lib.Utility.cadr;
import static com.github.chungkwong.jschememin.lib.Utility.car;
import static com.github.chungkwong.jschememin.lib.Utility.getInteractiveEnvironment;
import com.github.chungkwong.jschememin.type.*;
import java.io.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Load extends NativeLibrary{
	public static final Load INSTANCE=new Load();
	private static final ScmSymbol BEGIN=new ScmSymbol("begin");
	public Load(){
		super("scheme","load");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("load",new NativeProcedureDefault((o)->load(((ScmString)car(o)).getValue(),(SchemeEnvironment)cadr(o)),
			(o)->car(o),(o)->getInteractiveEnvironment()));
	}
	private static ScmObject load(String filename,SchemeEnvironment env){
		ScmPair content;
		try{
			Parser parser=new Parser(new Lex(new InputStreamReader(new FileInputStream(filename),"UTF-8"),false));
			content=new ScmPair(BEGIN,ScmList.toList(parser.getRemainingDatums()));
		}catch(FileNotFoundException|UnsupportedEncodingException ex){
			throw new RuntimeException();
		}
		return new Evaluator(env).eval(content);
	}
}
