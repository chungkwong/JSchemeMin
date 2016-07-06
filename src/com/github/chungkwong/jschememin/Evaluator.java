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
package com.github.chungkwong.jschememin;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Evaluator{
	private final Environment env;
	private final HashSet<Library> imported=new HashSet<>();
	private final boolean repl;
	private Continuation cont;
	private static final ScmSymbol ok=new ScmSymbol("ok"),fail=new ScmSymbol("fail");
	public Evaluator(Environment env,boolean repl){
		this.env=env;
		this.repl=repl;
	}
	public ScmObject eval(ScmObject expr){
		if(expr instanceof ScmPair){
			ScmObject car=((ScmPair)expr).getCar();

			return fail;
		}else
			return expr;
	}
	public ScmObject eval(Continuation cont){
		while(cont.hasNext())
			cont.evalNext(env);
		return cont.getValue();
	}
}
