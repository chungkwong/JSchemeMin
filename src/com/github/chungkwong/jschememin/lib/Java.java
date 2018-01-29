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
import static com.github.chungkwong.jschememin.lib.Utility.caddr;
import static com.github.chungkwong.jschememin.lib.Utility.cadr;
import static com.github.chungkwong.jschememin.lib.Utility.car;
import static com.github.chungkwong.jschememin.lib.Utility.cdr;
import com.github.chungkwong.jschememin.type.*;
import java.lang.reflect.*;
import java.math.*;
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
		addNativeProcedure("boolean->Boolean",(o)->new ScmJavaObject(((ScmBoolean)car(o)).isTrue()));
		addNativeProcedure("Boolean->boolean",(o)->ScmBoolean.valueOf((Boolean)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("symbol->String",(o)->new ScmJavaObject(((ScmSymbol)car(o)).getValue()));
		addNativeProcedure("string->String",(o)->new ScmJavaObject(((ScmString)car(o)).getValue()));
		addNativeProcedure("String->symbol",(o)->new ScmSymbol((String)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("String->string",(o)->new ScmString((String)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("integer->Integer",(o)->new ScmJavaObject(((ScmComplex)car(o)).intValueExact()));
		addNativeProcedure("Integer->integer",(o)->new ScmInteger((Integer)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("integer->BigInteger",(o)->new ScmJavaObject(((ScmComplex)car(o)).toScmInteger().getValue()));
		addNativeProcedure("BigInteger->integer",(o)->new ScmInteger((BigInteger)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("real->Double",(o)->new ScmJavaObject(((ScmComplex)car(o)).toScmReal().toDouble()));
		addNativeProcedure("Double->real",(o)->ScmReal.valueOf((Double)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("real->BigDecimal",(o)->new ScmJavaObject(((ScmFloatingPointNumber)((ScmComplex)car(o)).toScmReal().toInExact()).getValue()));
		addNativeProcedure("BigDecimal->real",(o)->new ScmFloatingPointNumber((BigDecimal)((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("bytevector->ByteArray",(o)->new ScmJavaObject(((ScmByteVector)car(o)).getByteArray()));
		addNativeProcedure("ByteArray->bytevector",(o)->new ScmByteVector((byte[])((ScmJavaObject)car(o)).getJavaObject()));
		addNativeProcedure("instanceof",(o)->is(o));
		addNativeProcedure("construct",(o)->construct(o));
		addNativeProcedure("invoke",(o)->invoke(o));
		addNativeProcedure("invoke-static",(o)->invokeStatic(o));
		addNativeProcedure("get",(o)->getField(o));
		addNativeProcedure("get-static",(o)->getStaticField(o));
		addNativeProcedure("set",(o)->setField(o));
		addNativeProcedure("set-static",(o)->setStaticField(o));
		addNativeProcedure("cast-to",(o)->cast(o));
		addDeriveFile("/com/github/chungkwong/jschememin/lib/java_derive.scm","scheme->java","java->scheme",
				"easy-construct","easy-invoke","easy-invoke-static");
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
			types[i]=args[i]==null?null:args[i].getClass();
		return types;
	}
	private static ScmObject is(ScmObject param) throws ClassNotFoundException{
		Object obj=((ScmJavaObject)car(param)).getJavaObject();
		Class type=forName(((ScmSymbol)cadr(param)).getValue());
		return ScmBoolean.valueOf(type.isInstance(obj));
	}
	private static ScmObject getField(ScmObject param) throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException, ClassNotFoundException{
		Object obj=((ScmJavaObject)car(param)).getJavaObject();
		Class cls=obj.getClass();
		String field=((ScmSymbol)cadr(param)).getValue();
		return new ScmJavaObject(cls.getField(field).get(obj));
	}
	private static ScmObject getStaticField(ScmObject param) throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException, ClassNotFoundException{
		Class cls=forName(((ScmSymbol)car(param)).getValue());
		String field=((ScmSymbol)cadr(param)).getValue();
		return new ScmJavaObject(cls.getField(field).get(null));
	}
	private static ScmObject setField(ScmObject param) throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException, ClassNotFoundException{
		Object obj=((ScmJavaObject)car(param)).getJavaObject();
		Class cls=obj.getClass();
		String field=((ScmSymbol)cadr(param)).getValue();
		Object val=((ScmJavaObject)caddr(param)).getJavaObject();
		cls.getField(field).set(obj,val);
		return caddr(param);
	}
	private static ScmObject setStaticField(ScmObject param) throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException, ClassNotFoundException{
		Class cls=forName(((ScmSymbol)car(param)).getValue());
		String field=((ScmSymbol)cadr(param)).getValue();
		Object val=((ScmJavaObject)caddr(param)).getJavaObject();
		cls.getField(field).set(null,val);
		return caddr(param);
	}
	private static ScmObject invoke(ScmObject param)throws ClassNotFoundException
			, NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException{
		Object obj=((ScmJavaObject)car(param)).getJavaObject();
		Class cls=obj.getClass();
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());
		Class[] paraType=toClassArray(arguments);
		Method m=(Method)selectMethod(cls.getMethods(),method,paraType);
		if(m!=null)
			try{
				return new ScmJavaObject(m.invoke(obj,adjustForVarargs(arguments,m)));
			}catch(Throwable ex){
				throw ScmError.toRuntimeException(ex);
			}
		else
			throw new NoSuchMethodException(method);
	}
	private static ScmObject invokeStatic(ScmObject param)throws ClassNotFoundException
			, NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException{
		Class cls=forName(((ScmSymbol)car(param)).getValue());
		String method=((ScmSymbol)cadr(param)).getValue();
		Object[] arguments=toArray(((ScmPair)param).getCddr());
		Class[] paraType=toClassArray(arguments);
		Method m=(Method)selectMethod(cls.getMethods(),method,paraType);
		if(m!=null)
			return new ScmJavaObject(m.invoke(null,adjustForVarargs(arguments,m)));
		else
			throw new NoSuchMethodException(method);
	}
	private static ScmObject construct(ScmObject param)throws ClassNotFoundException
			, NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, InstantiationException{
		Class cls=forName(((ScmSymbol)car(param)).getValue());
		Object[] arguments=toArray(cdr(param));
		Class[] paraType=toClassArray(arguments);
		Constructor m=(Constructor)selectMethod(cls.getConstructors(),cls.getName(),paraType);
		if(m!=null)
			return new ScmJavaObject(m.newInstance(adjustForVarargs(arguments,m)));
		else
			throw new NoSuchMethodException();
	}
	public static Object[] adjustForVarargs(Object[] args,Executable m){
		if(m.isVarArgs()){
			Object[] arguments=new Object[m.getParameterCount()];
			int len=arguments.length-1;
			System.arraycopy(args,0,arguments,0,len);
			arguments[len]=Arrays.copyOfRange(args,len,args.length,(Class)m.getParameterTypes()[len]);
			return arguments;
		}else{
			return args;
		}
	}
	private static ScmObject cast(ScmObject param) throws ClassNotFoundException{
		return new ScmJavaObject(forName(((ScmSymbol)cadr(param)).getValue()).cast(((ScmJavaObject)car(param)).getJavaObject()));
	}
	private static Executable selectMethod(Executable[] choices,String name,Class[] argsType){
		Executable best=null;
		Class<?>[] bestParameterTypes=null;
		boolean bestVarargs=false;
		for(Executable m:choices){
			if(!m.getName().equals(name))
				continue;
			Class<?>[] parameterTypes=m.getParameterTypes();
			boolean varargs=m.isVarArgs();
			if(isPossible(parameterTypes,varargs,argsType)){
				if(best==null||isBetterThan(parameterTypes,varargs,bestParameterTypes,bestVarargs)){
					best=m;
					bestParameterTypes=parameterTypes;
					bestVarargs=varargs;
				}else if(!isBetterThan(bestParameterTypes,bestVarargs,parameterTypes,varargs))
					return null;
			}
		}
		return best;
	}
	private static boolean isPossible(Class<?>[] paraType,boolean varargs,Class<?>[] argsType){
		if(varargs){
			if(argsType.length<paraType.length-1)
				return false;
			for(int i=0;i<paraType.length-1;i++)
				if(!canAssignFrom(paraType[i],argsType[i]))
					return false;
			for(int i=paraType.length-1;i<argsType.length;i++)
				if(!canAssignFrom(paraType[paraType.length-1].getComponentType(),argsType[i]))
					return false;
			return true;
		}else{
			if(argsType.length!=paraType.length)
				return false;
			for(int i=0;i<argsType.length;i++)
				if(!canAssignFrom(paraType[i],argsType[i]))
					return false;
			return true;
		}
	}
	private static boolean canAssignFrom(Class left,Class right){
		return right==null||left.isAssignableFrom(right)||(left.isPrimitive()&&boxTo(left,right))||(right.isPrimitive()&&boxTo(right,left));
	}
	private static boolean boxTo(Class left,Class right){
		return (left==int.class&&right==Integer.class)||
				(left==boolean.class&&right==Boolean.class)||
				(left==double.class&&right==Double.class)||
				(left==char.class&&right==Character.class)||
				(left==short.class&&right==Short.class)||
				(left==byte.class&&right==Byte.class)||
				(left==long.class&&right==Long.class)||
				(left==float.class&&right==Float.class);
	}
	private static boolean isBetterThan(Class<?>[] paraType1,boolean varargs1,Class<?>[] paraType2,boolean varargs2){
		if(varargs1!=varargs2)
			return varargs2;
		int i=0;
		for(;i<paraType1.length&&i<paraType2.length;i++)
			if(!paraType2[i].isAssignableFrom(paraType1[i]))
				return false;
		for(;i<paraType1.length;i++)
			if(!paraType2[paraType2.length-1].isAssignableFrom(paraType1[i]))
				return false;
		for(;i<paraType2.length;i++)
			if(!paraType2[i].isAssignableFrom(paraType1[paraType1.length-1]))
				return false;
		return true;
	}
	private static Class forName(String name) throws ClassNotFoundException{
		return Class.forName(name);
	}
}