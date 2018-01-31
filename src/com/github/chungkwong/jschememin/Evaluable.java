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
import com.github.chungkwong.jschememin.type.*;
/**
 * Represents expression types
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class Evaluable extends ScmObject{
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	/**
	 * To be called when such exprssion is evaluated
	 * @param env the environment where the exprssion is evaluated
	 * @param cont the execution stack
	 * @param pointer saved at last call
	 * @param param parameters given to the type
	 */
	public abstract void call(SchemeEnvironment env,Continuation cont,Object pointer,ScmPairOrNil param);
	/**
	 * Call and get the result in a new stack
	 * @param param
	 * @return
	 */
	public ScmObject call(ScmPairOrNil param){
		Continuation cont=new Continuation();
		cont.callInit(this,param,null);
		while(cont.hasNext())
			cont.evalNext();
		return cont.getCurrentValue();
	}
}
