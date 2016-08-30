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
public class Quasiquote extends BasicConstruct implements Primitive{
	private static final ScmSymbol QUASIQUOTE=new ScmSymbol("quasiquote");
	private static final ScmSymbol QUOTE=new ScmSymbol("quote");
	private static final ScmSymbol UNQUOTE=new ScmSymbol("unquote");
	private static final ScmSymbol UNQUOTE_SLICING=new ScmSymbol("unquote-splicing");
	public static final Quasiquote INSTANCE=new Quasiquote();
	private Quasiquote(){
		super(QUASIQUOTE);
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmPairOrNil param){
		ScmObject val=ScmList.toList(QUOTE,quasiquote(ScmList.first(param),1,env));
		cont.callTail(ExpressionEvaluator.INSTANCE,ScmList.singleton(val),env);
	}
	private static ScmObject quasiquote(ScmObject obj,int depth,Environment env){
		if(depth==0){
			return new Evaluator(env).eval(obj);//FIXME wrong hack
		}else if(obj instanceof ScmVector){
			ArrayList<ScmObject> vector=new ArrayList<>();
			((ScmVector)obj).stream().forEach((o)->processVector(o,vector,depth,env));
			return new ScmVector(vector);
		}else if(obj instanceof ScmPair){
			if(ScmList.first(obj).equals(UNQUOTE)){
				ScmObject content=quasiquote(ScmList.second(obj),depth-1,env);
				return depth==1?content:ScmList.toList(UNQUOTE,content);
			}else if(ScmList.first(obj).equals(QUASIQUOTE)){
				ScmObject content=quasiquote(ScmList.second(obj),depth+1,env);
				return ScmList.toList(QUASIQUOTE,content);
			}else if(ScmList.first(obj).equals(QUOTE)){
				ScmObject content=quasiquote(ScmList.second(obj),depth,env);
				return ScmList.toList(QUOTE,content);
			}else{
				ScmListBuilder buf=new ScmListBuilder();
				while(obj instanceof ScmPair){
					ScmObject curr=ScmList.first(obj);
					if(curr instanceof ScmSymbol&&(curr.equals(UNQUOTE)||curr.equals(QUASIQUOTE)||curr.equals(QUOTE))){
						buf.setLast(quasiquote(obj,depth,env));
						return buf.toList();
					}
					processList(curr,buf,depth,env);
					obj=((ScmPair)obj).getCdr();
				}
				if(!(obj instanceof ScmNil))
					buf.setLast(quasiquote(obj,depth,env));
				return buf.toList();
			}
		}else
			return obj;
	}
	private static void processVector(ScmObject obj,List<ScmObject> vector,int depth,Environment env){
		if(obj instanceof ScmPair&&ScmList.first(obj).equals(UNQUOTE_SLICING)){
			ScmObject content=quasiquote(ScmList.second(obj),depth-1,env);
			if(depth==1)
				ScmList.forEach(content,(o)->vector.add(o));
			else{
				vector.add(ScmList.toList(UNQUOTE_SLICING,content));
			}
		}else{
			vector.add(quasiquote(obj,depth,env));
		}
	}
	private static void processList(ScmObject obj,ScmListBuilder buf,int depth,Environment env){
		if(obj instanceof ScmPair&&ScmList.first(obj).equals(UNQUOTE_SLICING)){
			ScmObject content=quasiquote(ScmList.second(obj),depth-1,env);
			if(depth==1)
				ScmList.forEach(content,(o)->buf.add(o));
			else{
				buf.add(ScmList.toList(UNQUOTE_SLICING,content));
			}
		}else{
			buf.add(quasiquote(obj,depth,env));
		}
	}
}
