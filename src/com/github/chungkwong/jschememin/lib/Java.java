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
import java.lang.invoke.*;
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
		addNativeProcedure("null",(o)->new ScmJavaObject(null));
		addNativeProcedure("symbol->String",(o)->new ScmJavaObject(((ScmSymbol)car(o)).getValue()));
		addNativeProcedure("string->String",(o)->new ScmJavaObject(((ScmString)car(o)).getValue()));
		addNativeProcedure("String->symbol",(o)->new ScmSymbol((String)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("String->string",(o)->new ScmString((String)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("invoke",(o)->invoke(o));
		addNativeProcedure("invoke-static",(o)->invokeStatic(o));
		addNativeProcedure("cast-to",(o)->cast(o));
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
	private ScmObject invoke(ScmObject param) throws ClassNotFoundException{
		ScmObject subject=car(param);
		Object obj=((ScmJavaObject)subject).getJavaObject();
		Class cls=obj.getClass();
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());
		Object retValue;
		Class[] paraType=Arrays.stream(arguments).map((arg)->arg.getClass()).toArray(Class[]::new);
		try{
			retValue=MethodHandles.lookup().findVirtual(cls,method,MethodType.methodType(Object.class,paraType)).invoke(obj,arguments);
		}catch(Throwable ex){
			throw new RuntimeException(ex);
		}
		//retValue=cls.getMethod(method,paraType).invoke(obj,arguments);
		return new ScmJavaObject(retValue);
	}
	private ScmObject invokeStatic(ScmObject param) throws ClassNotFoundException{
		ScmObject subject=car(param);
		Class cls=Class.forName(((ScmSymbol)subject).getValue());
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());
		Object retValue;
		Class[] paraType=Arrays.stream(arguments).map((arg)->arg.getClass()).toArray(Class[]::new);
		try{
			retValue=MethodHandles.lookup().findStatic(cls,method,MethodType.methodType(Object.class,paraType)).invoke(arguments);
		}catch(Throwable ex){
			throw new RuntimeException(ex);
		}
		//retValue=cls.getMethod(method,paraType).invoke(obj,arguments);
		return new ScmJavaObject(retValue);
	}
	private ScmObject cast(ScmObject param) throws ClassNotFoundException{
		return new ScmJavaObject(Class.forName(((ScmSymbol)cadr(param)).getValue()).cast(((ScmJavaObject)car(param)).getJavaObject()));
	}
	public static void main(String[] args) throws Throwable{

	}
}
