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
import com.github.chungkwong.jschememin.type.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Time extends NativeLibrary{
	public static final Time INSTANCE=new Time();
	public Time(){
		super((ScmPair)ScmList.toList(new ScmString("scheme"),new ScmString("time")));
	}
	@Override
	protected void init(Library lib){
		addNativeProcedure("current-second",(o)->new ScmInteger(System.currentTimeMillis()/1000));
		addNativeProcedure("current-jiffy",(o)->new ScmInteger(System.nanoTime()));
		addNativeProcedure("jiffies-per-second",(o)->new ScmInteger(1000000000));
	}
}
