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
import static com.github.chungkwong.jschememin.lib.Utility.car;
import static com.github.chungkwong.jschememin.lib.Utility.emergencyExit;
import static com.github.chungkwong.jschememin.lib.Utility.exit;
import static com.github.chungkwong.jschememin.lib.Utility.getEnvironmentVariable;
import static com.github.chungkwong.jschememin.lib.Utility.getEnvironmentVariables;
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ProcessContext extends NativeLibrary{
	public static final ProcessContext INSTANCE=new ProcessContext();
	public ProcessContext(){
		super("scheme","process-context");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("command-line",(o)->Main.COMMAND_LINE);
		addNativeProcedure("exit",new NativeProcedureDefault((o)->{exit(o);return null;},(o)->ScmBoolean.TRUE));
		addNativeProcedure("emergency-exit",new NativeProcedureDefault((o)->{emergencyExit(o);return null;},(o)->ScmBoolean.TRUE));
		addNativeProcedure("get-environment-variable",(o)->getEnvironmentVariable((ScmString)car(o)));
		addNativeProcedure("get-environment-variables",(o)->getEnvironmentVariables());
	}
}
