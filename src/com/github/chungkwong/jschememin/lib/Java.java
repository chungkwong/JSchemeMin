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
package com.github.chungkwong.jschememin.lib;
import com.github.chungkwong.jschememin.*;
import static com.github.chungkwong.jschememin.lib.Utility.cadr;
import static com.github.chungkwong.jschememin.lib.Utility.car;
import com.github.chungkwong.jschememin.type.*;
import java.lang.reflect.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Java extends NativeLibrary{
	public static final Java INSTANCE=new Java();
	private Java(){
		super("java");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("invoke",(o)->invoke(o));
	}
	private static Object[] toArray(ScmObject obj){
		int length=ScmList.getLength(obj);
		Object[] array=new Object[length];
		for(int i=0;i<length;i++){
			array[i]=((ScmJavaObject)((ScmPair)obj).getCar()).getJavaObject();
			obj=((ScmPair)obj).getCdr();
		}
		return array;
	}
	protected ScmObject invoke(ScmObject param) throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException{
		ScmObject subject=car(param);
		Object obj;
		Class cls;
		if(subject instanceof ScmSymbol){
			cls=Class.forName(((ScmSymbol)subject).getValue());
			obj=null;
		}else{
			obj=((ScmJavaObject)subject).getJavaObject();
			cls=obj.getClass();
		}
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());;
		Object retValue;
		Class[] paraType=Arrays.stream(arguments).map((arg)->arg.getClass()).toArray(Class[]::new);
		retValue=cls.getMethod(method,paraType).invoke(obj,arguments);
		return new ScmJavaObject(retValue);
	}
}
