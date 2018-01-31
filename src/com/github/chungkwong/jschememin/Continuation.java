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
 * Evaluate stack
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Continuation extends ScmObject{
	private final Stack<Evaluable> actives;
	private final Stack<Object> pointers;
	private final Stack<SchemeEnvironment> environments;
	private ScmPairOrNil arguments;
	/**
	 * Create a stack
	 */
	public Continuation(){
		this.pointers=new Stack<>();
		this.actives=new Stack<>();
		this.environments=new Stack<>();
	}
	/**
	 * Create a srack
	 * @param actives
	 * @param pointers
	 * @param environments
	 */
	public Continuation(Stack<Evaluable> actives,Stack<Object> pointers,Stack<SchemeEnvironment> environments){
		this.pointers=pointers;
		this.actives=actives;
		this.environments=environments;
	}
	/**
	 * First call
	 * @param proc
	 * @param arguments
	 * @param env
	 */
	public void callInit(Evaluable proc,ScmPairOrNil arguments,SchemeEnvironment env){
		actives.push(proc);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	/**
	 * First call
	 * @param proc
	 * @param arguments
	 * @param env
	 */
	public void callInit(Evaluable proc,ScmObject arguments,SchemeEnvironment env){
		callInit(proc,ScmList.toList(arguments),env);
	}
	/**
	 * Subsequence call
	 * @param proc
	 * @param pointer
	 * @param arguments
	 * @param env
	 */
	public void call(Evaluable proc,Object pointer,ScmPairOrNil arguments,SchemeEnvironment env){
		actives.push(proc);
		pointers.pop();
		pointers.push(pointer);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	/**
	 * Subsequence call
	 * @param proc
	 * @param pointer
	 * @param arguments
	 * @param env
	 */
	public void call(Evaluable proc,Object pointer,ScmObject arguments,SchemeEnvironment env){
		call(proc,pointer,ScmList.toList(arguments),env);
	}
	/**
	 * Tail call
	 * @param proc
	 * @param arguments
	 * @param env
	 */
	public void callTail(Evaluable proc,ScmPairOrNil arguments,SchemeEnvironment env){
		actives.pop();
		pointers.pop();
		environments.pop();
		actives.push(proc);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	/**
	 * Tail call
	 * @param proc
	 * @param arguments
	 * @param env
	 */
	public void callTail(Evaluable proc,ScmObject arguments,SchemeEnvironment env){
		callTail(proc,ScmList.toList(arguments),env);
	}
	/**
	 * Change the current expression type
	 * @param proc
	 */
	public void replaceCurrent(Evaluable proc){
		actives.pop();
		actives.push(proc);
	}
	/**
	 * return values
	 * @param retValue
	 */
	public void ret(ScmPairOrNil retValue){
		actives.pop();
		pointers.pop();
		environments.pop();
		this.arguments=retValue;
	}
	/**
	 * Return a value
	 * @param retValue
	 */
	public void ret(ScmObject retValue){
		ret(ScmList.toList(retValue));
	}
	/**
	 * Reset the srack to a known status
	 * @param cont known status
	 */
	public void reset(Continuation cont){
		ArrayList<Evaluable> actives2=new ArrayList<>();
		ArrayList<Object> pointers2=new ArrayList<>();
		ArrayList<SchemeEnvironment> environments2=new ArrayList<>();
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
	/**
	 * Go next step
	 */
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
	/**
	 * Check if not finished
	 * @return
	 */
	public boolean hasNext(){
		return !pointers.isEmpty();
	}
	/**
	 * Get current value
	 * @return
	 */
	public ScmObject getCurrentValue(){
		return ScmList.first(arguments);
	}
	/**
	 * Get current values
	 * @return
	 */
	public ScmPairOrNil getCurrentValueRaw(){
		return arguments;
	}
	/**
	 * Get current Evaluable
	 * @return
	 */
	public Evaluable getCurrentEvaluable(){
		return actives.peek();
	}
	/**
	 * Get current pointer
	 * @return
	 */
	public Object getCurrentPointer(){
		return pointers.peek();
	}
	/**
	 * Get current environment
	 * @return
	 */
	public SchemeEnvironment getCurrentEnvironment(){
		return environments.peek();
	}
	/**
	 * Get the size of the stack
	 * @return
	 */
	public int getLevel(){
		return actives.size();
	}
	/**
	 * Clone the stack
	 * @return
	 */
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
	/**
	 * Get the current error handler and its environment
	 * @return
	 */
	public ScmPair getErrorHandler(){
		int index=actives.search(WithExceptionHandler.INSTANCE);
		if(index>0)
			return new ScmPair((ScmObject)pointers.get(pointers.size()-index),environments.get(pointers.size()-index));
		else
			return null;
	}
	/**
	 * Transfer control to the error handler
	 */
	public void removeUntilErrorHandler(){
		while(!actives.isEmpty()&&!(actives.peek() instanceof WithExceptionHandler)){
			actives.pop();
			pointers.pop();
			environments.pop();
		}
	}
	@Override
	public String toString(){
		StringBuilder buf=new StringBuilder();
		for(int i=0;i<actives.size();i++)
			buf.append(actives.get(i).getClass().getSimpleName()).append(':').append(pointers.get(i)).append('\n');
		buf.append(arguments);
		return buf.toString();
	}
}