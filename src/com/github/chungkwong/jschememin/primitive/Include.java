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
import java.io.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Include extends PrimitiveType{
	public static final Include INSTANCE=new Include("include",false);
	public static final Include INSTANCE_CI=new Include("include-ci",true);
	private final boolean foldingCase;
	private Include(String name,boolean foldingCase){
		super(new ScmSymbol(name));
		this.foldingCase=foldingCase;
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject expr){
		if(pointer==null){
			call(env,cont,expr,null);
		}else if(((ScmPair)pointer).getCdr()==ScmNil.NIL){
			cont.callTail(ExpressionEvaluator.INSTANCE,getFileContent(((ScmString)((ScmPair)expr).getCar()).getValue(),foldingCase));
		}else{
			cont.call(ExpressionEvaluator.INSTANCE,((ScmPair)pointer).getCdr(),getFileContent(((ScmString)((ScmPair)expr).getCar()).getValue(),foldingCase));
		}
	}
	private ScmPair getFileContent(String file,boolean foldingCase){
		try{
			Parser parser=new Parser(new Lex(new InputStreamReader(new FileInputStream(file),"UTF-8"),foldingCase));
			return new ScmPair(new ScmSymbol("begin"),ScmPair.toList(parser.getRemainingDatums()));
		}catch(FileNotFoundException|UnsupportedEncodingException ex){
			throw new RuntimeException();
		}
	}

}