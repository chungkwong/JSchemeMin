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
import com.github.chungkwong.jschememin.type.ScmSyntaxRules.SyntaxRule;
import java.io.*;
import java.util.*;
import java.util.stream.*;
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
		rules.add(new SyntaxRule(rule.getCdar(),rule.getCadr()));
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append("(syntax-rules ");
		buf.append(ellipsis.getValue());
		buf.append(literals.stream().map((id)->id.getValue()).collect(Collectors.joining(" "," (",") ")));
		for(SyntaxRule rule:rules){
			buf.append('(').append(rule.pattern).append(' ').append(rule.template).append(')');
		}
		buf.append(')');
		return buf.toString();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	public ScmObject transform(ScmPairOrNil argument,Environment env){
		for(SyntaxRule rule:rules){
			ScmObject transformed=rule.apply(argument,env);
			if(transformed!=null){
				return transformed;
			}
		}
		throw new SyntaxException();
	}
	public static void main(String[] args) throws IOException{
		BufferedReader in=new BufferedReader(new InputStreamReader(System.in));
		String s;
		Environment env=new Environment(false);
		while((s=in.readLine())!=null){
			ScmSyntaxRules rules=new ScmSyntaxRules((ScmPair)new Parser(s).nextDatum(),env);
			System.out.println(rules);
			System.out.println(rules.transform((ScmPair)new Parser(in.readLine()).nextDatum(),env));
		}
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
		private void collectPatternVariables(ScmObject patt,HashMap<ScmSymbol,CapturedObjects> bind){
			if(patt instanceof ScmSymbol){
				bind.put((ScmSymbol)patt,new CapturedObjects());
			}else if(patt instanceof ScmPair){
				collectPatternVariables(((ScmPair)patt).getCar(),bind);
				collectPatternVariables(((ScmPair)patt).getCdr(),bind);
			}else if(patt instanceof ScmVector){
				((ScmVector)patt).stream().forEach((p)->collectPatternVariables(p,bind));
			}
		}
		private boolean matchIdentifier(ScmObject expr,ScmSymbol patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(literals.contains(patt)){
				return expr instanceof ScmSymbol&&((expr.equals(patt)&&!defEnv.containsKey(patt)&&!env.containsKey((ScmSymbol)expr))
						||(defEnv.containsKey(patt)&&env.containsKey((ScmSymbol)expr)&&defEnv.get((ScmSymbol)patt).equals(env.get((ScmSymbol)expr))));
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
			if(split<patt.getLength()){
				index.push();
				collectPatternVariables(patt.get(split),bind);
				for(int i=split;i<split+exp.getLength()-patt.getLength()+2;i++){
					if(!match(exp.get(i),patt.get(split),bind,env,index))
						return false;
					index.advance();
				}
				index.pop();
			}
			for(int i=split+2;i<patt.getLength();i++)
				if(!match(exp.get(i+exp.getLength()-patt.getLength()),patt.get(i),bind,env,index))
					return false;
			return true;
		}
		private boolean matchList(ScmObject expr,ScmObject patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			while(patt instanceof ScmPair){
				ScmObject sub=((ScmPair)patt).getCar();
				if(((ScmPair)patt).getCdr()instanceof ScmPair&&((ScmPair)patt).getCadr().equals(ellipsis)){
					index.push();
					int count=ScmList.getLength(expr)-ScmList.getLength(patt)+2;
					if(count==0)
						collectPatternVariables(sub,bind);
					while(--count>=0&&expr instanceof ScmPair){
						if(!match(ScmList.first(expr),sub,bind,env,index))
							return false;
						expr=((ScmPair)expr).getCdr();
						index.advance();
					}
					index.pop();
					patt=((ScmPair)patt).getCdr();
				}else{
					if(!(expr instanceof ScmPair)||!match(ScmList.first(expr),sub,bind,env,index))
						return false;
					expr=((ScmPair)expr).getCdr();
				}
				patt=((ScmPair)patt).getCdr();
			}
			if(!(patt instanceof ScmNil)){
				return match(expr,patt,bind,env,index);
			}else
				return expr instanceof ScmNil;
		}
		private boolean match(ScmObject expr,ScmObject patt,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(patt instanceof ScmSymbol){
				return matchIdentifier(expr,(ScmSymbol)patt,bind,env,index);
			}else if(patt instanceof ScmPairOrNil){
				return matchList(expr,(ScmPairOrNil)patt,bind,env,index);
			}else if(patt instanceof ScmVector){
				return matchVector(expr,(ScmVector)patt,bind,env,index);
			}else if(patt.isSelfevaluating()){
				return patt.equals(expr);
			}else{
				throw new SyntaxException();
			}
		}
		private ScmObject transform(ScmObject temp,HashMap<ScmSymbol,CapturedObjects> bind,boolean ellipsed,Environment env,MultiIndex index){
			if(temp instanceof ScmSymbol)
				return transformSymbol((ScmSymbol)temp,bind,env,index);
			else if(temp.isSelfevaluating())
				return temp;
			else if(temp instanceof ScmPair){
				if(((ScmPair)temp).getCar().equals(ellipsis)&&!ellipsed)
					return transform(((ScmPair)temp).getCadr(),bind,true,env,index);
				else
					return transformList(temp,bind,ellipsed,env,index);
			}else if(temp instanceof ScmVector){
				return transformVector((ScmVector)temp,bind,ellipsed,env,index);
			}else
				return temp;
		}
		private ScmObject transformSymbol(ScmSymbol temp,HashMap<ScmSymbol,CapturedObjects> bind,Environment env,MultiIndex index){
			if(bind.containsKey(temp))
				return bind.get(temp).get(index);
			/*if(env.containsKey(temp)){
				if(defEnv.containsKey(temp)){
					return ScmList.toList(quote(Eval.INSTANCE),quote(temp),quote(defEnv));
				}else{
					ScmSymbol rename=defEnv.getUnusedVariable();
					bind.put(temp,new Rename(rename));
					return rename;
				}
			}else
				return temp;*/
			Optional<ScmObject> defVal=defEnv.getOptional(temp);
			if(!defVal.isPresent()){
				ScmSymbol rename=defEnv.getUnusedVariable();
				bind.put(temp,new Rename(rename));
				return rename;
			}else{
				return ScmList.toList(Quote.INSTANCE.getKeyword(),defVal.get());
			}/* if(env.containsKey(temp)){
//				return ScmList.toList(quote(Eval.INSTANCE),quote(temp),quote(defEnv));
				return ScmList.toList(Quote.INSTANCE.getKeyword(),defVal.get());
			}else
				return temp;*/
		}
		private ScmObject transformVector(ScmVector temp,HashMap<ScmSymbol,CapturedObjects> bind,boolean ellipsed,Environment env,MultiIndex index){
			ArrayList<ScmObject> list=new ArrayList<>();
			for(int i=0;i<temp.getLength();i++){
				if(i+1<temp.getLength()&&temp.get(i+1).equals(ellipsis)&&!ellipsed){
					index.push();
					try{
						while(true){
							list.add(transform(temp.get(i),bind,ellipsed,env,index));
							index.advance();
						}
					}catch(RuntimeException ex){
					}
					index.pop();
					++i;
				}else{
					list.add(transform(temp.get(i),bind,ellipsed,env,index));
				}
			}
			return new ScmVector(list);
		}
		private ScmObject transformList(ScmObject temp,HashMap<ScmSymbol,CapturedObjects> bind,boolean ellipsed,Environment env,MultiIndex index){
			ScmListBuilder buf=new ScmListBuilder();
			while(temp instanceof ScmPair){
				ScmObject sub=((ScmPair)temp).getCar();
				if(((ScmPair)temp).getCdr()instanceof ScmPair&&((ScmPair)temp).getCadr().equals(ellipsis)&&!ellipsed){
					index.push();
					try{
						while(true){
							buf.add(transform(sub,bind,ellipsed,env,index));
							index.advance();
						}
					}catch(RuntimeException ex){
					}
					index.pop();
					temp=((ScmPair)temp).getCdr();
				}else{
					buf.add(transform(sub,bind,ellipsed,env,index));
				}
				temp=((ScmPair)temp).getCdr();
			}
			if(!(temp instanceof ScmNil)){
				buf.setLast(transform(temp,bind,ellipsed,env,index));
			}
			return buf.toList();
		}
		private ScmObject apply(ScmPairOrNil argument,Environment env){
			HashMap<ScmSymbol,CapturedObjects> bind=new HashMap<>();
			if(match(argument,pattern,bind,env,new MultiIndex()))
				return transform(template,bind,false,env,new MultiIndex());
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
				int curr=indices.get(i);
				while(curr>=toSearch.size())
					toSearch.add(new ArrayList());
				toSearch=(List)toSearch.get(curr);
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
	class Rename extends CapturedObjects{
		private final ScmSymbol renameTo;
		public Rename(ScmSymbol renameTo){
			this.renameTo=renameTo;
		}
		@Override
		public ScmObject get(MultiIndex index){
			return renameTo;
		}
	}
	static class MultiIndex{
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