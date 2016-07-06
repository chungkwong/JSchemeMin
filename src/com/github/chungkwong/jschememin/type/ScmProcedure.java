package com.github.chungkwong.jschememin.type;
import com.github.chungkwong.jschememin.*;
public final class ScmProcedure extends ScmObject implements Evaluable{
	private final Environment parent;
	private final ScmObject formal;
	private final ScmPairOrNil body;
	public ScmProcedure(ScmObject formal,ScmPairOrNil body,Environment parent){
		this.formal=formal;
		this.body=body;
		this.parent=parent;
	}
	@Override
	public String toExternalRepresentation(){
		return "'procedure";
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	@Override
	public void call(Environment dynamicEnv,Continuation cont,Object pointer,ScmObject param){
		Environment env=extendEnvironment((ScmPairOrNil)param);
		//TODO
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
	private Environment extendEnvironment(ScmPairOrNil param){
		Environment env=new Environment(parent);
		if(formal instanceof ScmSymbol){
			env.add((ScmSymbol)formal,param);
		}else if(formal instanceof ScmPair){
			ScmPair remainingFormal=(ScmPair)formal;
			while(true){
				env.add((ScmSymbol)remainingFormal.getCar(),((ScmPair)param).getCar());
				param=(ScmPairOrNil)((ScmPair)param).getCdr();
				ScmObject next=remainingFormal.getCdr();
				if(next instanceof ScmPair){
					remainingFormal=(ScmPair)next;
				}else if(next instanceof ScmNil){
					break;
				}else if(next instanceof ScmSymbol){
					env.add((ScmSymbol)next,param);
					break;
				}else
					throw new SyntaxException();
			}
		}else if(formal instanceof ScmNil){

		}else
			throw new SyntaxException();
		return env;
	}
}