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
import java.lang.management.*;
/**
 * Correspoding to the library (jschememin) in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class JSchemeMin extends NativeLibrary{
	public static final JSchemeMin INSTANCE=new JSchemeMin();
	static final NativeProcedure LIBRARY_EXISTS=(o)->ScmBoolean.valueOf(LibraryManager.hasLibrary((ScmPair)car(o)));
	private JSchemeMin(){
		super("jschememin");
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("library-exists?",LIBRARY_EXISTS);
		addNativeProcedure("thread-clock",(o)->ScmInteger.valueOf(ManagementFactory.getThreadMXBean().getCurrentThreadCpuTime()));
		addNativeProcedure("memory-total",(o)->ScmInteger.valueOf(Runtime.getRuntime().totalMemory()));
		addNativeProcedure("memory-free",(o)->ScmInteger.valueOf(Runtime.getRuntime().freeMemory()));
		addDeriveFile("/com/github/chungkwong/jschememin/lib/jschememin_derive.scm",
				"duration","count","total-time","profile-lambda","profile-record","profile-records");
	}

}
