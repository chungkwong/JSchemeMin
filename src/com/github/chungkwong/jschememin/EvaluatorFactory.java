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
import java.util.*;
import java.util.stream.*;
import javax.script.*;
/**
 * An implementation of ScriptEngineFactory
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class EvaluatorFactory implements ScriptEngineFactory{
	/**
	 * The factory for jSchemeMin
	 */
	public static final EvaluatorFactory INSTANCE=new EvaluatorFactory();
	private static final HashMap<String,String> PARAMETERS=new HashMap<>();
	static{
		PARAMETERS.put("ScriptEngine.ENGINE","JSchemeMin");
		PARAMETERS.put("ScriptEngine.ENGINE_VERSION","0.0.1");
		PARAMETERS.put("ScriptEngine.NAME","JSchemeMin");
		PARAMETERS.put("ScriptEngine.LANGUAGE","scheme");
		PARAMETERS.put("ScriptEngine.LANGUAGE_VERSION","7");
	}
	private  EvaluatorFactory(){

	}
	@Override
	public String getEngineName(){
		return PARAMETERS.get("ScriptEngine.ENGINE");
	}
	@Override
	public String getEngineVersion(){
		return PARAMETERS.get("ScriptEngine.ENGINE_VERSION");
	}
	@Override
	public List<String> getExtensions(){
		return Arrays.asList("scm");
	}
	@Override
	public List<String> getMimeTypes(){
		return Arrays.asList("text/x-scheme");
	}
	@Override
	public List<String> getNames(){
		return Arrays.asList(PARAMETERS.get("ScriptEngine.NAME"));
	}
	@Override
	public String getLanguageName(){
		return PARAMETERS.get("ScriptEngine.LANGUAGE_NAME");
	}
	@Override
	public String getLanguageVersion(){
		return PARAMETERS.get("ScriptEngine.LANGUAGE_VERSION");
	}
	@Override
	public Object getParameter(String key){
		return PARAMETERS.get(key);
	}
	@Override
	public String getMethodCallSyntax(String obj,String m,String... args){
		return Arrays.stream(args).collect(Collectors.joining(" ","("+m,")"));
	}
	@Override
	public String getOutputStatement(String toDisplay){
		return "(write-string "+new ScmString(toDisplay).toExternalRepresentation()+")";
	}
	@Override
	public String getProgram(String... statements){
		return Arrays.stream(statements).collect(Collectors.joining(" ","(begin ",")"));
	}
	@Override
	public Evaluator getScriptEngine(){
		return new Evaluator(true);
	}
}
