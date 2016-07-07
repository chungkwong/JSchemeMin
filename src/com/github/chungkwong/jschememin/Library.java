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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Library{
	private final ScmPair name;
	private final HashMap<ScmSymbol,ScmSymbol> export;
	private final Environment internal;
	public Library(ScmPair name,HashMap<ScmSymbol,ScmSymbol> export,Environment internal){
		this.name=name;
		this.export=export;
		this.internal=internal;
	}
	public ScmPair getName(){
		return name;
	}
	public Set<ScmSymbol> getExportSet(){
		return export.keySet();
	}
	public void exportTo(Environment env,HashMap<ScmSymbol,ScmSymbol> importset){
		importset.forEach((ex,im)->env.add(im,internal.get(export.get(ex))));
	}
}