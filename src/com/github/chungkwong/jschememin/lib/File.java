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
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class File extends NativeLibrary{
	public static final File INSTANCE=new File();
	public File(){
		super((ScmPair)ScmList.toList(new ScmString("scheme"),new ScmString("file")));
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("file-exists?",(o)->ScmBoolean.valueOf(new java.io.File(((ScmString)car(o)).getValue()).exists()));
		addNativeProcedure("file-delete",(o)->ScmBoolean.valueOf(new java.io.File(((ScmString)car(o)).getValue()).delete()));
		addNativeProcedure("open-input-file",(o)->new ScmTextualInputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("open-binary-input-file",(o)->new ScmBinaryInputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("open-output-file",(o)->new ScmTextualOutputPort(((ScmString)car(o)).getValue()));
		addNativeProcedure("open-binary-output-file",(o)->new ScmBinaryOutputPort(((ScmString)car(o)).getValue()));
	}
}
