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
	public ScmSyntaxRules(ScmPair spec){
		if(spec.getCar() instanceof ScmSymbol){
			ellipsis=(ScmSymbol)spec.getCar();
			spec=(ScmPair)spec.getCdr();
		}else
			ellipsis=ELLIPSIS;
		((ScmPairOrNil)spec.getCar()).forEach((id)->literals.add((ScmSymbol)id));
		((ScmPairOrNil)spec.getCdr()).forEach((rule)->addSyntaxRule((ScmPair)rule));
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
	public ScmObject tranform(ScmPairOrNil argument){
		return rules.stream().filter((rule)->rule.getPattern().match(argument)).findFirst().get().getTemplate().apply(argument);
	}
	public ScmObject applyTemplate(ScmObject template,HashMap<ScmSymbol,ScmObject> binding){

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
		public Optional<HashMap<ScmSymbol,ScmObject>> match(ScmObject arg,Environment env){
			HashMap<ScmSymbol,ScmObject> bind=new HashMap<>();
			return match(arg,pattern,bind,env)?Optional.of(bind):Optional.empty();
		}
		private boolean matchIdentifier(ScmObject expr,ScmSymbol patt,HashMap<ScmSymbol,ScmObject> bind,Environment env){
			if(literals.contains((ScmSymbol)patt)){
				return expr.equals((ScmSymbol)patt);
			}else if(patt.equals(WILDCARD))
				return true;
			else{
				if(bind.containsKey((ScmSymbol)patt))
					bind.put((ScmSymbol)patt,new ScmPair(expr,bind.get((ScmSymbol)patt)));
				else{
					bind.put((ScmSymbol)patt,expr);
				}
				return true;
			}
		}
		private boolean matchVector(ScmObject expr,ScmVector patt,HashMap<ScmSymbol,ScmObject> bind,Environment env){
			if(!(expr instanceof ScmVector))
				return false;
			ScmVector exp=(ScmVector)expr;
			int split=patt.getLength();
			for(int i=0;i<patt.getLength();i++){
				if(ellipsis.equals(patt.get(i)))
					split=i-1;
			}
			for(int i=0;i<split;i++)
				if(!match(expr,patt.get(i),bind,env))
					return false;

			return true;
		}
		private boolean matchList(ScmObject expr,ScmPairOrNil patt,HashMap<ScmSymbol,ScmObject> bind,Environment env){

		}
		private boolean match(ScmObject expr,ScmObject patt,HashMap<ScmSymbol,ScmObject> bind,Environment env){
			if(patt.isSelfevaluating()){
				return patt.equals(expr);
			}else if(patt instanceof ScmSymbol){
				return matchIdentifier(expr,(ScmSymbol)patt,bind,env);
			}else if(patt instanceof ScmPairOrNil){
				return matchList(expr,(ScmPairOrNil)patt,bind,env);
			}else if(patt instanceof ScmVector){
				return matchVector(expr,(ScmVector)patt,bind,env);
			}else{
				throw new SyntaxException();
			}
		}
		private ScmObject apply(HashMap<ScmSymbol,ScmObject> bind){

		}
		private ScmObject apply(ScmObject temp,HashMap<ScmSymbol,ScmObject> bind,boolean ellipsed,Environment env){
			if(temp instanceof ScmSymbol)
				return transformSymbol((ScmSymbol)temp,bind,env);
			else if(temp instanceof ScmPair){
				if(((ScmPair)temp).getCar().equals(ellipsis))
					return apply(((ScmPair)temp).getCdr(),bind,true);

			}else if(temp instanceof ScmVector){
				ScmVector vector=(ScmVector)temp;
				for(int i=0;i<vector.getLength();i++){

				}
			}else
				return temp;
		}
		private ScmObject transformSymbol(ScmSymbol temp,HashMap<ScmSymbol,ScmObject> bind,Environment env){
			if(bind.containsKey(temp))
				return bind.get(temp);
			if(env.getOptional(temp).isPresent()){
				ScmSymbol rename=env.getUnusedVariable();
				bind.put(temp,rename);
				return rename;
			}
			return temp;
		}
	}
}