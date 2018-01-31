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
import com.github.chungkwong.jschememin.type.*;
import java.util.function.*;
/**
 * NativeProcedure with default arguments
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class NativeProcedureDefault implements NativeProcedure{
	private final NativeProcedure proc;
	private final Function<ScmPairOrNil,ScmObject>[] def;
	/**
	 *
	 * @param proc the NativeProcedure
	 * @param def the functions being used to generate default argument
	 */
	public NativeProcedureDefault(NativeProcedure proc,Function<ScmPairOrNil,ScmObject>... def){
		this.proc=proc;
		this.def=def;
	}
	@Override
	public ScmObject call(ScmObject param) throws Exception{
		if(ScmList.getLength(param)<def.length){
			ScmListBuilder filled=new ScmListBuilder();
			int i=0;
			while(param instanceof ScmPair){
				filled.add(ScmList.first(param));
				param=((ScmPair)param).getCdr();
				++i;
			}
			for(;i<def.length;i++)
				filled.add(def[i].apply((ScmPairOrNil)filled.toList()));
			param=filled.toList();
		}
		return proc.call(param);
	}

}
