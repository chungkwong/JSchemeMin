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
package com.github.chungkwong.jschememin.type;
import com.github.chungkwong.jschememin.*;
import com.github.chungkwong.jschememin.primitive.*;
import java.io.*;
import java.util.*;
import java.util.function.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class HygieneTransformer{

	public static ScmObject transform(ScmObject obj,Environment env){
		return unStamp(rename(expand(stamp(obj,0),env,1),env));
	}
	private static ScmSymbolWithTimeStamp S(ScmSymbol id,int n){
		return new ScmSymbolWithTimeStamp(id.getValue(),n);
	}
	private static ScmObject expand(ScmObject tsstree,Environment env,int n){
		if(tsstree instanceof ScmPair){
			if(((ScmPair)tsstree).getCar()instanceof ScmSymbol){
				Optional<ScmObject> optional=env.getOptional((ScmSymbol)((ScmPair)tsstree).getCar());
				if(optional.isPresent()){
					if(optional.get() instanceof Lambda){
						return new ScmPair(ScmList.first(tsstree),new ScmPair(ScmList.second(tsstree),
								expand(((ScmPair)tsstree).getCddr(),env,n)));
					}else if(optional.get() instanceof Quote){
						return tsstree;
					}else if(optional.get() instanceof ScmSyntaxRules){
						return expand(stamp(((ScmSyntaxRules)optional.get()).transform((ScmPairOrNil)((ScmPair)tsstree).getCdr(),env),n),env,n+1);
					}
				}
			}
			return new ScmPair(expand(((ScmPair)tsstree).getCar(),env,n),expand(((ScmPair)tsstree).getCdr(),env,n));
		}else{
			return tsstree;
		}
	}
	private static ScmObject replace(ScmSymbolWithTimeStamp tsvar,ScmSymbol var,ScmObject tsstree,Environment env){
		if(tsstree instanceof ScmPair){
			if(((ScmPair)tsstree).getCar()instanceof ScmSymbol){
				Optional<ScmObject> optional=env.getOptional((ScmSymbol)((ScmPair)tsstree).getCar());
				if(optional.isPresent()){
					if(optional.get() instanceof Lambda){
						ScmObject tmp=ScmList.second(tsstree);
						while(tmp instanceof ScmPair){
							if(tsvar.equalsWithStamp(((ScmPair)tmp).getCar()))
								return new ScmPair(ScmList.first(tsstree),new ScmPair(ScmList.second(tsstree),
										replace(tsvar,var,((ScmPair)tsstree).getCddr(),env)));
							tmp=((ScmPair)tmp).getCdr();
						}
						if(tsvar.equalsWithStamp(tmp)){
							return new ScmPair(ScmList.first(tsstree),new ScmPair(ScmList.second(tsstree),
									replace(tsvar,var,((ScmPair)tsstree).getCddr(),env)));
						}
					}else if(optional.get() instanceof Quote){
						return tsstree;
					}
				}
			}
			return new ScmPair(replace(tsvar,var,((ScmPair)tsstree).getCar(),env),
					replace(tsvar,var,((ScmPair)tsstree).getCdr(),env));
		}else if(tsvar.equalsWithStamp(tsstree)){
			return var;
		}else if(tsstree instanceof ScmVector){
			ArrayList<ScmObject> vector=new ArrayList<>(((ScmVector)tsstree).getLength());
			((ScmVector)tsstree).stream().forEach((o)->vector.add(replace(tsvar,var,o,env)));
			return new ScmVector(vector);
		}
		return tsstree;
	}
	private static ScmObject rename(ScmObject tsstree,Environment env){
		if(tsstree instanceof ScmPair){
			if(((ScmPair)tsstree).getCar()instanceof ScmSymbol){
				Optional<ScmObject> optional=env.getOptional((ScmSymbol)((ScmPair)tsstree).getCar());
				if(optional.isPresent()){
					if(optional.get() instanceof Lambda){
						ScmObject body=((ScmPair)tsstree).getCddr();
						if(ScmList.second(tsstree)instanceof ScmSymbolWithTimeStamp){
							ScmSymbol v=env.getUnusedVariable();
							return new ScmPair(ScmList.first(tsstree),new ScmPair(v,
									rename(replace((ScmSymbolWithTimeStamp)ScmList.second(tsstree),v,body,env),env)));
						}else if(ScmList.second(tsstree)instanceof ScmPair){
							ScmListBuilder buf=new ScmListBuilder();
							ScmObject args=(ScmPair)ScmList.second(tsstree);
							while(args instanceof ScmPair){
								ScmObject var=((ScmPair)args).getCar();
								if(var instanceof ScmSymbolWithTimeStamp){
									ScmSymbol v=env.getUnusedVariable();
									body=replace((ScmSymbolWithTimeStamp)var,v,body,env);
									buf.add(v);
								}else{
									buf.add(var);
								}
								args=((ScmPair)args).getCdr();
							}
							return new ScmPair(ScmList.first(tsstree),new ScmPair(buf.toList(),rename(body,env)));
						}
					}else if(optional.get() instanceof Quote){
						return tsstree;
					}
				}
			}
			return new ScmPair(rename(((ScmPair)tsstree).getCar(),env),rename(((ScmPair)tsstree).getCdr(),env));
		}else if(tsstree instanceof ScmVector){
			ArrayList<ScmObject> vector=new ArrayList<>(((ScmVector)tsstree).getLength());
			((ScmVector)tsstree).stream().forEach((o)->vector.add(rename(o,env)));
			return new ScmVector(vector);
		}else{
			return tsstree;
		}
	}
	private static ScmObject stamp(ScmObject tsstree,int n){
		return walkTree(tsstree,(o)->o instanceof ScmSymbol&&!(o instanceof ScmSymbolWithTimeStamp)?
				S((ScmSymbol)o,n):o);
	}
	private static ScmObject unStamp(ScmObject tsstree){
		return walkTree(tsstree,(o)->o instanceof ScmSymbolWithTimeStamp?
				new ScmSymbol(((ScmSymbolWithTimeStamp)o).getValue()):o);
	}
	private static ScmObject walkTree(ScmObject tree,Function<ScmObject,ScmObject> transform){
		if(tree instanceof ScmPair){
			return new ScmPair(walkTree(((ScmPair)tree).getCar(),transform),walkTree(((ScmPair)tree).getCdr(),transform));
		}else if(tree instanceof ScmVector){
			ArrayList<ScmObject> vector=new ArrayList<>(((ScmVector)tree).getLength());
			((ScmVector)tree).stream().forEach((o)->vector.add(walkTree(o,transform)));
			return new ScmVector(vector);
		}else{
			return transform.apply(tree);
		}
	}
	private static class ScmSymbolWithTimeStamp extends ScmSymbol{
		private final int ts;
		public ScmSymbolWithTimeStamp(String id,int ts){
			super(id);
			this.ts=ts;
		}
		public int getTimeStamp(){
			return ts;
		}
		public boolean equalsWithStamp(Object obj){
			return obj instanceof ScmSymbolWithTimeStamp&&super.equals(obj)
					&&ts==((ScmSymbolWithTimeStamp)obj).ts;
		}
	}
	public static void main(String[] args) throws IOException{
		Environment env=new Environment(true);
		BufferedReader in=new BufferedReader(new InputStreamReader(System.in));
		/*String s;
		while((s=in.readLine())!=null){
			System.out.println(transform(new Parser(s).nextDatum(),env));
		}*/
		System.out.println(transform(new Parser("(or)").nextDatum(),env));
		System.out.println(transform(new Parser("(or 1)").nextDatum(),env));
		System.out.println(transform(new Parser("(or 1 2)").nextDatum(),env));
		System.out.println(transform(new Parser("(or 1 2 3)").nextDatum(),env));
	}
}