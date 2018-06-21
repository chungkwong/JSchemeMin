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
import com.github.chungkwong.jschememin.lib.*;
import java.lang.reflect.*;
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
			cont.ret(fromType(method.invoke(obj,correctType(ScmList.asStream(param).toArray(),method)),method.getReturnType()));
		}catch(Exception ex){
			throw new RuntimeException("Expect Evaluable:"+obj,ex);
		}
	}
	private static Object[] correctType(Object[] args,Method method){
		Class<?>[] types=method.getParameterTypes();
		if(method.isVarArgs()){
			for(int i=0;i<types.length-1;i++){
				args[i]=toType(args[i],types[i]);
			}
			for(int i=types.length-1;i<args.length;i++){
				args[i]=toType(args[i],types[types.length-1]);
			}
		}else{
			for(int i=0;i<types.length;i++){
				args[i]=toType(args[i],types[i]);
			}
		}
		return Java.adjustForVarargs(args,method);
	}
	private static Object toType(Object obj,Class cls){
		if(cls.isAssignableFrom(obj.getClass())){
			return obj;
		}else if(cls==String.class&&obj instanceof ScmString){
			return ((ScmString)obj).getValue();
		}else if((cls==Integer.class||cls==int.class)&&obj instanceof ScmComplex){
			return ((ScmComplex)obj).intValueExact();
		}else if((cls==Boolean.class||cls==boolean.class)&&obj instanceof ScmBoolean){
			return ((ScmBoolean)obj).isTrue();
		}else{
			return obj;
		}
	}
	private static ScmObject fromType(Object obj,Class cls){
		if(cls==Void.TYPE){
			return ScmNil.NIL;
		}
		if(cls==String.class){
			return new ScmString((String)obj);
		}else if((cls==Integer.class||cls==int.class)){
			return new ScmInteger((Integer)obj);
		}else if((cls==Boolean.class||cls==boolean.class)){
			return ScmBoolean.valueOf((Boolean)obj);
		}else{
			return toScmObject(obj);
		}
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
