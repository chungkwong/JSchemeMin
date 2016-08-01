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
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmSyntaxRules extends ScmObject{
	private static final ScmSymbol ELLIPSIS=new ScmSymbol("...");
	private static final ScmSymbol WILDCARD=new ScmSymbol("_");
	private final List<SyntaxRule> rules=new ArrayList<>();
	private final ScmSymbol ellipsis;
	private final HashSet<ScmSymbol> literals=new HashSet<>();
	private final Environment defEnv;
	public ScmSyntaxRules(ScmPair spec,Environment defEnv){
		if(spec.getCar() instanceof ScmSymbol){
			ellipsis=(ScmSymbol)spec.getCar();
			spec=(ScmPair)spec.getCdr();
		}else
			ellipsis=ELLIPSIS;
		ScmList.forEach((ScmPairOrNil)spec.getCar(),(id)->literals.add((ScmSymbol)id));
		ScmList.forEach((ScmPairOrNil)spec.getCdr(),(rule)->addSyntaxRule((ScmPair)rule));
		this.defEnv=defEnv;
	}
	private void addSyntaxRule(ScmPair rule){
		rules.add(new SyntaxRule(rule.getCar(),rule.getCadr()));
	}
	@Override
	public String toExternalRepresentation(){
		return "'macro";
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	public ScmObject transform(ScmPairOrNil argument,Environment env){
		for(SyntaxRule rule:rules){
			ScmObject transformed=rule.apply(argument,env);
			if(transformed!=null)
				return transformed;
		}
		throw new RuntimeException();
	}
	class SyntaxRule{
		final ScmObject pattern;
		final ScmObject template;
		public SyntaxRule(ScmObject pattern,ScmObject template){
			this.pattern=pattern;
			this.template=template;
		}
		public ScmObject getPattern(){
			return pattern;
		}
		public ScmObject getTemplate(){
			return template;
		}
		private boolean matchIdentifier(ScmObject expr,ScmSymbol patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(literals.contains(patt)){
				return expr instanceof ScmSymbol&&((expr.equals(patt)&&!defEnv.containsKey(patt)&&!env.containsKey((ScmSymbol)expr))
						||(env.containsKey(patt)&&env.containsKey((ScmSymbol)expr)&&env.get((ScmSymbol)expr).equals(env.get(patt))));
			}else if(patt.equals(WILDCARD))
				return true;
			else{
				if(!bind.containsKey(patt))
					bind.put(patt,new CapturedObjects());
				bind.get(patt).add(index,expr);
				return true;
			}
		}
		private boolean matchVector(ScmObject expr,ScmVector patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(!(expr instanceof ScmVector))
				return false;
			ScmVector exp=(ScmVector)expr;
			int split=patt.getLength();
			for(int i=0;i<patt.getLength();i++){
				if(ellipsis.equals(patt.get(i)))
					split=i-1;
			}
			for(int i=0;i<split;i++)
				if(!match(exp.get(i),patt.get(i),bind,env,index))
					return false;
			index.push();
			for(int i=split;i<split+exp.getLength()-patt.getLength()+2;i++){
				if(!match(exp.get(i),patt.get(split),bind,env,index))
					return false;
				index.advance();
			}
			index.pop();
			for(int i=split+2;i<patt.getLength();i++)
				if(!match(exp.get(i+exp.getLength()-patt.getLength()),patt.get(i),bind,env,index))
					return false;
			return true;
		}
		private boolean matchList(ScmObject expr,ScmPairOrNil patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){

		}
		private boolean match(ScmObject expr,ScmObject patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(patt.isSelfevaluating()){
				return patt.equals(expr);
			}else if(patt instanceof ScmSymbol){
				return matchIdentifier(expr,(ScmSymbol)patt,bind,env,index);
			}else if(patt instanceof ScmPairOrNil){
				return matchList(expr,(ScmPairOrNil)patt,bind,env,index);
			}else if(patt instanceof ScmVector){
				return matchVector(expr,(ScmVector)patt,bind,env,index);
			}else{
				throw new SyntaxException();
			}
		}
		private ScmObject apply(ScmObject temp,HashMap<ScmSymbol,CapturedObjects> bind,boolean ellipsed,Environment env,MultiIndex index){
			if(temp instanceof ScmSymbol)
				return transformSymbol((ScmSymbol)temp,bind,env,index);
			else if(temp.isSelfevaluating())
				return temp;
			else if(temp instanceof ScmPair){
				if(((ScmPair)temp).getCar().equals(ellipsis))
					return apply(((ScmPair)temp).getCdr(),bind,true,env,index);

			}else if(temp instanceof ScmVector){
				ScmVector vector=(ScmVector)temp;
				ArrayList<ScmObject> list=new ArrayList<>();
				for(int i=0;i<vector.getLength();i++){
					if(i+1<vector.getLength()&&vector.get(i+1).equals(ellipsis)){
						index.push();
						try{
							while(true){
								list.add(apply(vector.get(i),bind,ellipsed,env,index));
								index.advance();
							}
						}catch(RuntimeException ex){

						}
						index.pop();
						++i;
					}else{
						list.add(apply(vector.get(i),bind,ellipsed,env,index));
					}
				}
				return new ScmVector(list);
			}else
				return temp;
		}
		private ScmObject transformSymbol(ScmSymbol temp,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(bind.containsKey(temp))
				return bind.get(temp).get(index);
			if(env.getOptional(temp).isPresent()){
				ScmSymbol rename=env.getUnusedVariable();
				bind.put(temp,new CapturedObjects());
				bind.get(temp).add(index,rename);
				return rename;
			}
			return temp;
		}
		private ScmObject apply(ScmPairOrNil argument,Environment env){
			HashMap<ScmSymbol,CapturedObjects> bind=new HashMap<>();
			if(match(argument,pattern,bind,env,new MultiIndex()))
				return apply(template,bind,false,env,new MultiIndex());
			else
				return null;
		}
	}
	class CapturedObjects{
		private final List objects;
		public CapturedObjects(){
			this.objects=new ArrayList();
		}
		public void add(MultiIndex index,ScmObject obj){
			List toSearch=objects;
			Stack<Integer> indices=index.getIndices();
			for(int i=0;i<indices.size()-1;i++){
				while(i>=toSearch.size())
					toSearch.add(new ArrayList());
				toSearch=(List)toSearch.get(i);
			}
			toSearch.add(indices.get(indices.size()-1),obj);
		}
		public ScmObject get(MultiIndex index){
			Object toSearch=objects;
			for(Integer i:index.getIndices())
				toSearch=((List)toSearch).get(i);
			return (ScmObject)toSearch;
		}
	}
	class MultiIndex{
		private final Stack<Integer> indices=new Stack<>();
		public MultiIndex(){
			indices.push(0);
		}
		public void push(){
			indices.push(0);
		}
		public void pop(){
			indices.pop();
		}
		public void advance(){
			indices.push(indices.pop()+1);
		}
		Stack<Integer> getIndices(){
			return indices;
		}
		@Override
		public boolean equals(Object obj){
			return obj instanceof MultiIndex&&((MultiIndex)obj).indices.equals(indices);
		}
		@Override
		public int hashCode(){
			int hash=5;
			hash=43*hash+Objects.hashCode(this.indices);
			return hash;
		}
	}
}