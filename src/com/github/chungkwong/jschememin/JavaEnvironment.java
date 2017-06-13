/*
 * Copyright (C) 2017 Chan Chung Kwong <1m02math@126.com>
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
import com.github.chungkwong.jschememin.lib.*;
import com.github.chungkwong.jschememin.type.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.function.*;
import javax.script.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class JavaEnvironment extends Environment{
	private final ScriptContext parent;
	public JavaEnvironment(ScriptContext parent,boolean repl){
		super(repl);
		this.parent=parent;
	}
	@Override
	public Optional<ScmObject> getOptional(ScmSymbol id){
		return Optional.ofNullable(getSelfOptional(id));
	}
	@Override
	public ScmObject getSelfOptional(ScmSymbol id){
		Object value=parent.getAttribute(id.getValue());
		return value==null?null:pack(value);
	}
	private static NativeProcedure toNativeProcedure(Object obj){
		Method method=obj.getClass().getDeclaredMethods()[0];
		return (args)->fromType(method.invoke(obj,correctType(ScmList.asStream((ScmPairOrNil)args).map(JavaEnvironment::unpack).toArray(),method)),method.getReturnType());
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
		if(cls==String.class&&obj instanceof ScmString){
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
		}if(cls==String.class){
			return new ScmString((String)obj);
		}else if((cls==Integer.class||cls==int.class)){
			return new ScmInteger((Integer)obj);
		}else if((cls==Boolean.class||cls==boolean.class)){
			return ScmBoolean.valueOf((Boolean)obj);
		}else{
			return pack(obj);
		}
	}
	@Override
	public void set(ScmSymbol id,ScmObject obj){
		int scope=parent.getAttributesScope(id.getValue());
		if(scope!=-1)
			parent.setAttribute(id.getValue(),unpack(obj),scope);
		if(isREPL())
			add(id,obj);
	}
	@Override
	public void add(ScmSymbol id,ScmObject obj){
		parent.setAttribute(id.getValue(),unpack(obj),parent.getScopes().get(0));
	}
	private static Object unpack(ScmObject obj){
		if(obj instanceof ScmJavaObject)
			return ((ScmJavaObject)obj).getJavaObject();
		else
			return obj;
	}
	private static ScmObject pack(Object value){
		if(value instanceof ScmObject){
			return (ScmObject)value;
		}else if(Arrays.stream(value.getClass().getInterfaces()).anyMatch((cls)->cls.isAnnotationPresent(FunctionalInterface.class))){
			return new NativeEvaluable(toNativeProcedure(value));
		}else{
			return new ScmJavaObject(value);
		}
	}
	@Override
	public void remove(ScmSymbol id){
		int scope=parent.getAttributesScope(id.getValue());
		if(scope!=-1){
			parent.removeAttribute(id.getValue(),scope);
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
		System.out.println(engine.eval("(string-length (up \"hello\"))"));
	}
}
