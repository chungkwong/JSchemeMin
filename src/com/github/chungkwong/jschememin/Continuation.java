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
	private ScmObject arguments;
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
	public void callInit(Evaluable proc,ScmObject arguments,Environment env){
		actives.push(proc);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	public void call(Evaluable proc,Object pointer,ScmObject arguments,Environment env){
		actives.push(proc);
		pointers.pop();
		pointers.push(pointer);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	public void callTail(Evaluable proc,ScmObject arguments,Environment env){
		actives.pop();
		pointers.pop();
		environments.pop();
		actives.push(proc);
		pointers.push(null);
		environments.push(env);
		this.arguments=arguments;
	}
	public void replaceCurrent(Evaluable proc){
		actives.pop();
		actives.push(proc);
	}
	public void ret(ScmObject retValue){
		actives.pop();
		pointers.pop();
		environments.pop();
		this.arguments=retValue;
	}
	public void reset(Continuation cont){
		actives.clear();
		pointers.clear();
		environments.clear();
		actives.addAll(cont.actives);
		pointers.addAll(cont.pointers);
		environments.addAll(cont.environments);
		arguments=cont.arguments;//TODO dynamic-wind
	}
	public void evalNext(){
		try{
			actives.peek().call(environments.peek(),this,pointers.peek(),arguments);
		}catch(RuntimeException ex){
			ex.printStackTrace();
			while(!actives.isEmpty()&&!(actives.peek() instanceof WithExceptionHandler)){
				actives.pop();
				pointers.pop();
				environments.pop();
			}
			if(actives.isEmpty())
				throw ex;
			arguments=ScmError.toScmObject(ex);
			pointers.push(new WithExceptionHandler.ErrorInfo((ScmObject)pointers.pop()));
		}
	}
	public boolean hasNext(){
		return !pointers.isEmpty();
	}
	public ScmObject getCurrentValue(){
		return arguments;
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
	public ScmObject getErrorHandler(){
		int index=actives.search(WithExceptionHandler.INSTANCE);
		return index>=0?(ScmObject)pointers.get(pointers.size()-index):null;
	}
}