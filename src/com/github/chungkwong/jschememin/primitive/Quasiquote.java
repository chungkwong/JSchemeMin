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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Quasiquote extends BasicConstruct implements Primitive{
	public static final Quasiquote INSTANCE=new Quasiquote();
	private static final ScmSymbol QUASIQUOTE=new ScmSymbol("quasiquote");
	private static final ScmSymbol UNQUOTE=new ScmSymbol("unquote");
	private static final ScmSymbol UNQUOTE_SLICING=new ScmSymbol("unquote-slicing");
	private Quasiquote(){
		super(QUASIQUOTE);

	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject param){
		cont.ret(quasiquote(((ScmPair)param).getCar(),1));
	}
	public static ScmObject quasiquote(ScmObject obj,int depth){
		/*if(obj.isSelfevaluating())
			return obj;
		else if(obj instanceof ScmVector){
			ArrayList<ScmObject> vector=new ArrayList<>();

		}else if(obj instanceof ScmPair){
			if(((ScmPair)obj).getCar().equals(UNQUOTE)){

			}else if(((ScmPair)obj).getCar().equals(UNQUOTE_SLICING)){

			}else if(((ScmPair)obj).getCar().equals(QUASIQUOTE)){

			}else{

			}
		}else
			throw new RuntimeException();*/
		return obj;//TODO
	}
}
