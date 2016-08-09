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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Write extends NativeLibrary{
	public static final Write INSTANCE=new Write();
	public Write(){
		super("scheme","write");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("write",new NativeProcedureDefault((o)->((ScmTextualOutputPort)cadr(o)).write(car(o)),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT));
		addNativeProcedure("write-shared",new NativeProcedureDefault((o)->((ScmTextualOutputPort)cadr(o)).writeShared(car(o)),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT));
		addNativeProcedure("write-simple",new NativeProcedureDefault((o)->((ScmTextualOutputPort)cadr(o)).writeSimple(car(o)),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT));
		addNativeProcedure("display",new NativeProcedureDefault((o)->((ScmTextualOutputPort)cadr(o)).display(car(o)),
			(o)->car(o),(o)->ScmPort.CURRENT_OUTPUT));
		}
}
