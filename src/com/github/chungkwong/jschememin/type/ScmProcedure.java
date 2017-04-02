package com.github.chungkwong.jschememin.type;
import com.github.chungkwong.jschememin.*;
public final class ScmProcedure extends Evaluable{
	private final SchemeEnvironment parent;
	private final ScmObject formal;
	private final ScmPair body;
	public ScmProcedure(ScmObject formal,ScmPair body,SchemeEnvironment parent){
		this.formal=formal;
		this.body=body;
		this.parent=parent;
	}
	@Override
	public String toExternalRepresentation(){
		StringBuilder buf=new StringBuilder();
		buf.append("(lambda ").append(formal.toExternalRepresentation()).append(' ').append(body.getCar().toExternalRepresentation());
		ScmPair tmp=body;
		while(tmp.getCdr() instanceof ScmPair){
			tmp=(ScmPair)tmp.getCdr();
			buf.append(' ').append(tmp.getCar().toExternalRepresentation());
		}
		buf.append(')');
		return buf.toString();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	@Override
	public void call(SchemeEnvironment dynamicEnv,Continuation cont,Object pointer,ScmPairOrNil param){
		if(pointer==null){
			call(dynamicEnv,cont,new Backtrack(extendEnvironment((ScmPairOrNil)param),body),null);
		}else{
			assert pointer instanceof Backtrack;
			Backtrack b=(Backtrack)pointer;
			ScmObject next=b.getRemaining().getCdr();
			if(next==ScmNil.NIL){
				cont.callTail(ExpressionEvaluator.INSTANCE,b.getRemaining().getCar(),b.getEnvironment());
			}else if(next instanceof ScmPair){
				cont.call(ExpressionEvaluator.INSTANCE,new Backtrack(b.getEnvironment(),(ScmPair)next),b.getRemaining().getCar(),b.getEnvironment());
			}else
				throw new SyntaxException();
		}
	}
	@Override
	public boolean equalsValue(ScmObject obj){
		return this==obj;
	}
	private SchemeEnvironment extendEnvironment(ScmPairOrNil param){
		SchemeEnvironment env=new SchemeEnvironment(parent);
		if(formal instanceof ScmSymbol){
			env.add((ScmSymbol)formal,param);
		}else if(formal instanceof ScmPair){
			ScmPair remainingFormal=(ScmPair)formal;
			while(true){
				env.add((ScmSymbol)remainingFormal.getCar(),ScmList.first(param));
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
	static class Backtrack{
		private final SchemeEnvironment env;
		private final ScmPair remaining;
		public Backtrack(SchemeEnvironment env,ScmPair remaining){
			this.env=env;
			this.remaining=remaining;
		}
		public SchemeEnvironment getEnvironment(){
			return env;
		}
		public ScmPair getRemaining(){
			return remaining;
		}
	}
}