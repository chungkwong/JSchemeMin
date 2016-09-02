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
import java.util.stream.*;
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
	private static Class[] toClassArray(Object[] args){
		Class[] types=new Class[args.length];
		for(int i=0;i<args.length;i++)
			types[i]=args[i].getClass();
		return types;
	}
	private ScmObject invoke(ScmObject param) throws ClassNotFoundException, NoSuchMethodException{//TODO Improve performance
		Object obj=((ScmJavaObject)car(param)).getJavaObject();
		Class cls=obj.getClass();
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());
		Class[] paraType=toClassArray(arguments);
		ArrayList<Object> args=new ArrayList<>(arguments.length+1);
		args.add(obj);
		Arrays.stream(arguments).forEach((o)->args.add(o));
		for(Class retType:getPossibleReturnType(cls,method))
			try{
				return new ScmJavaObject(MethodHandles.lookup().findVirtual(cls,method,MethodType.methodType(retType,paraType))
						.invokeWithArguments(args));
			}catch(Throwable ex){

			}
		throw new NoSuchMethodException(method);
	}
	private ScmObject invokeStatic(ScmObject param) throws ClassNotFoundException, NoSuchMethodException{//TODO Improve performance
		Class cls=Class.forName(((ScmSymbol)car(param)).getValue());
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());
		Class[] paraType=toClassArray(arguments);
		for(Class retType:getPossibleReturnType(cls,method))
			try{
				return new ScmJavaObject(MethodHandles.lookup().findStatic(cls,method,MethodType.methodType(retType,paraType)).invokeWithArguments(Arrays.asList(arguments)));
			}catch(Throwable ex){

			}
		throw new NoSuchMethodException(method);
	}
	private ScmObject cast(ScmObject param) throws ClassNotFoundException{
		return new ScmJavaObject(Class.forName(((ScmSymbol)cadr(param)).getValue()).cast(((ScmJavaObject)car(param)).getJavaObject()));
	}
	private static Set<Class> getPossibleReturnType(Class cls,String method){
		return Arrays.stream(cls.getMethods()).filter((m)->m.getName().equals(method)).map((m)->m.getReturnType()).collect(Collectors.toSet());
	}
	public static void main(String[] args) throws NoSuchMethodException, IllegalAccessException{
		System.err.println(MethodHandles.lookup().findStatic(Collections.class,"singleton",MethodType.genericMethodType(1)));
	}
}
