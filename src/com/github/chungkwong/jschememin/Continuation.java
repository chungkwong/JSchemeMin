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
import com.github.chungkwong.jschememin.primitive.*;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Continuation extends ScmObject{
	private final Stack<Evaluable> actives;
	private final Stack<Object> pointers;
	private final Stack<Environment> environments;
	private ScmPairOrNil arguments;
	public Continuation(){
		this.pointers=new Stack<>();
		this.actives=new Stack<>();
		this.environments=new Stack<>();
	}
	public Continuation(Stack<Evaluable> actives,Stack<Object> pointers,Stack<Environment> environments){
		this.pointers=pointers;
		this.actives=actives;
		this.environments=environments;
	}
	public void callInit(Evaluable proc,ScmPairOrNil arguments,Environment env){
		actives.push(proc);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	public void callInit(Evaluable proc,ScmObject arguments,Environment env){
		callInit(proc,ScmList.singleton(arguments),env);
	}
	public void call(Evaluable proc,Object pointer,ScmPairOrNil arguments,Environment env){
		actives.push(proc);
		pointers.pop();
		pointers.push(pointer);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	public void call(Evaluable proc,Object pointer,ScmObject arguments,Environment env){
		call(proc,pointer,ScmList.singleton(arguments),env);
	}
	public void callTail(Evaluable proc,ScmPairOrNil arguments,Environment env){
		actives.pop();
		pointers.pop();
		environments.pop();
		actives.push(proc);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	public void callTail(Evaluable proc,ScmObject arguments,Environment env){
		callTail(proc,ScmList.singleton(arguments),env);
	}
	public void replaceCurrent(Evaluable proc){
		actives.pop();
		actives.push(proc);
	}
	public void ret(ScmPairOrNil retValue){
		actives.pop();
		pointers.pop();
		environments.pop();
		this.arguments=retValue;
	}
	public void ret(ScmObject retValue){
		ret(ScmList.singleton(retValue));
	}
	public void reset(Continuation cont){
		ArrayList<Evaluable> actives2=new ArrayList<>();
		ArrayList<Object> pointers2=new ArrayList<>();
		ArrayList<Environment> environments2=new ArrayList<>();
		for(int i=cont.actives.size()-1;i>=0;i--){
			if(cont.actives.get(i)instanceof DynamicWind&&cont.pointers.get(i)instanceof DynamicWind.Backtrack){
				DynamicWind.Backtrack track=(DynamicWind.Backtrack)cont.pointers.get(i);
				if(track.isStarted()){
					actives2.add(track.getBefore());
					pointers2.add(null);
					environments2.add(cont.environments.get(i));
				}
			}
		}
		for(int i=0;i<actives.size();i++){
			if(actives.get(i)instanceof DynamicWind&&pointers.get(i)instanceof DynamicWind.Backtrack){
				DynamicWind.Backtrack track=(DynamicWind.Backtrack)pointers.get(i);
				if(track.isStarted()){
					actives2.add(track.getAfter());
					pointers2.add(null);
					environments2.add(environments.get(i));
				}
			}
		}
		actives.clear();
		pointers.clear();
		environments.clear();
		actives.addAll(actives2);
		pointers.addAll(pointers2);
		environments.addAll(environments2);
		while(hasNext())
			evalNext();
		actives.addAll(cont.actives);
		pointers.addAll(cont.pointers);
		environments.addAll(cont.environments);
		arguments=cont.arguments;
	}
	public void evalNext(){
		try{
			actives.peek().call(environments.peek(),this,pointers.peek(),arguments);
		}catch(RuntimeException ex){
			//ex.printStackTrace();
			if(hasNext())
				callTail(Raise.INSTANCE,ScmError.toScmObject(ex),environments.peek());
			else
				throw ex;
		}catch(UncaughtExceptionError ex){
			throw ex.getCause();
		}
	}
	public boolean hasNext(){
		return !pointers.isEmpty();
	}
	public ScmObject getCurrentValue(){
		return ScmList.first(arguments);
	}
	public Evaluable getCurrentEvaluable(){
		return actives.peek();
	}
	public Object getCurrentPointer(){
		return pointers.peek();
	}
	public Environment getCurrentEnvironment(){
		return environments.peek();
	}
	public Continuation getCopy(){
		return new Continuation((Stack)actives.clone(),(Stack)pointers.clone(),(Stack)environments.clone());
	}
	@Override
	public String toExternalRepresentation(){
		return actives.toString();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	public ScmPair getErrorHandler(){
		int index=actives.search(WithExceptionHandler.INSTANCE);
		if(index>0)
			return new ScmPair((ScmObject)pointers.get(pointers.size()-index),environments.get(pointers.size()-index));
		else
			return null;
	}
	public void removeUntilErrorHandler(){
		while(!actives.isEmpty()&&!(actives.peek() instanceof WithExceptionHandler)){
			actives.pop();
			pointers.pop();
			environments.pop();
		}
	}
}