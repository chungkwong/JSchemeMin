/*
 * Copyright (C) 2018 Chan Chung Kwong
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
import java.util.function.*;
import javax.script.*;
import org.junit.*;
/**
 *
 * @author Chan Chung Kwong
 */
public class EmbedTest{
	public EmbedTest(){
	}
	@Test
	public void testJavaFunction() throws ScriptException{
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
		Assert.assertEquals(engine.eval("(up \"heLlo\")"),new ScmString("HELLO"));
	}
}
