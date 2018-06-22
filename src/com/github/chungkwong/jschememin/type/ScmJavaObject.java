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
package com.github.chungkwong.jschememin.type;
import com.github.chungkwong.jschememin.*;
import java.lang.reflect.*;
import java.math.*;
import java.util.*;
import java.util.function.*;
import javax.script.*;
/**
 * Represents the Scheme's type that wrap Java's type
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmJavaObject extends Evaluable{
	private final Object obj;
	/**
	 * Wrap a Java's Object
	 *
	 * @param obj
	 */
	public ScmJavaObject(Object obj){
		this.obj=obj;
	}
	/**
	 * Get the Java's object
	 *
	 * @return
	 */
	public Object getJavaObject(){
		return obj;
	}
	@Override
	public boolean equals(Object o){
		return o instanceof ScmJavaObject&&Objects.equals(((ScmJavaObject)o).obj,obj);
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=53*hash+Objects.hashCode(this.obj);
		return hash;
	}
	@Override
	public String toExternalRepresentation(){
		return ScmList.toList(new ScmSymbol("java"),new ScmString(Objects.toString(obj))).toExternalRepresentation();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
	@Override
	public void call(SchemeEnvironment env,Continuation cont,Object pointer,ScmPairOrNil param){
		try{
			Optional<Class<?>> function=Arrays.stream(obj.getClass().getInterfaces()).filter((i)->i.isAnnotationPresent(FunctionalInterface.class)).findAny();
			Method method=function.get().getDeclaredMethods()[0];
			cont.ret(toScmObject(method.invoke(obj,correctType(ScmList.asStream(param).toArray(),method))));
		}catch(Exception ex){
			throw new RuntimeException("Expect Evaluable:"+obj,ex);
		}
	}
	/**
	 * Convert scheme objects to java objects for invocation
	 *
	 * @param args the arguments
	 * @param method to be invoked
	 * @return the java objects
	 */
	public static Object[] correctType(Object[] args,Executable method){
		Class<?>[] types=method.getParameterTypes();
		if(method.isVarArgs()){
			Object[] arguments=new Object[method.getParameterCount()];
			for(int i=0;i<types.length-1;i++){
				arguments[i]=toJavaObject(args[i],types[i]);
			}
			Object[] remaining=new Object[args.length-types.length+1];
			for(int i=types.length-1, j=0;i<args.length;i++,j++){
				remaining[j]=toJavaObject(args[i],types[types.length-1]);
			}
			arguments[arguments.length-1]=remaining;
			return arguments;
		}else{
			for(int i=0;i<types.length;i++){
				args[i]=toJavaObject(args[i],types[i]);
			}
			return args;
		}
	}
	/**
	 * Convert a Scheme object to Object
	 *
	 * @param obj the Java object
	 * @param cls the target type
	 * @return the Scheme object
	 */
	private static Object toJavaObject(Object obj,Class cls){
		if(obj==null||cls.isAssignableFrom(obj.getClass())){
			return obj;
		}else if(obj instanceof ScmJavaObject){
			return ((ScmJavaObject)obj).getJavaObject();
		}else if(obj instanceof ScmComplex&&Number.class.isAssignableFrom(cls)){
			if(cls==int.class||cls==Integer.class){
				return ((ScmComplex)obj).getReal().toScmInteger().getValue().intValue();
			}else if(cls==byte.class||cls==Byte.class){
				return ((ScmComplex)obj).getReal().toScmInteger().getValue().byteValue();
			}else if(cls==short.class||cls==Short.class){
				return ((ScmComplex)obj).getReal().toScmInteger().getValue().shortValue();
			}else if(cls==long.class||cls==Long.class){
				return ((ScmComplex)obj).getReal().toScmInteger().getValue().longValue();
			}else if(cls==BigInteger.class){
				return ((ScmComplex)obj).getReal().toScmInteger().getValue();
			}else if(cls==float.class||cls==Float.class){
				return (float)((ScmComplex)obj).getReal().toDouble();
			}else if(cls==double.class||cls==Double.class){
				return ((ScmComplex)obj).getReal().toDouble();
			}
		}
		return ScmJavaObject.toJavaObject((ScmObject)obj);
	}
	/**
	 * Convert a Scheme object to Java object
	 *
	 * @param obj the Scheme object
	 * @return the Java object
	 */
	public static ScmObject toScmObject(Object obj){
		if(obj==null){
			return ScmNil.NIL;
		}else if(obj instanceof String){
			return new ScmString((String)obj);
		}else if(obj instanceof Boolean){
			return ScmBoolean.valueOf((Boolean)obj);
		}else if(obj instanceof Byte[]){
			return new ScmByteVector((byte[])obj);
		}else{
			return new ScmJavaObject(obj);
		}
	}
	/**
	 * Convert a Java object to Scheme object
	 *
	 * @param obj the Java object
	 * @return the Scheme object
	 */
	public static Object toJavaObject(ScmObject obj){
		if(obj instanceof ScmNil){
			return null;
		}else if(obj instanceof ScmString){
			return ((ScmString)obj).getValue();
		}else if(obj instanceof ScmBoolean){
			return ((ScmBoolean)obj).isTrue();
		}else if(obj instanceof ScmByteVector){
			return ((ScmByteVector)obj).getByteArray();
		}else if(obj instanceof ScmJavaObject){
			return ((ScmJavaObject)obj).getJavaObject();
		}else{
			return obj;
		}
	}
	public static void main(String[] args) throws ScriptException{
		ScriptEngine engine=EvaluatorFactory.INSTANCE.getScriptEngine();
		SimpleBindings bindings=new SimpleBindings();
		Function<String,String> f=new Function<String,String>(){
			@Override
			public String apply(String t){
				return t.toUpperCase();
			}
		};
		bindings.put("up",f);
		bindings.put("str","hello");
		engine.setBindings(bindings,ScriptContext.GLOBAL_SCOPE);
		System.out.println(engine.eval("(up \"hello\")"));
	}
}
