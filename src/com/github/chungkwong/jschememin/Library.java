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
 * Library in scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Library extends ScmObject{
	private final ScmPair name;
	private final HashMap<ScmSymbol,ScmSymbol> export;
	private final SchemeEnvironment internal;
	/**
	 * Create a library
	 * @param name the name
	 * @param export the export name mapping
	 * @param internal internal environment
	 */
	public Library(ScmPair name,HashMap<ScmSymbol,ScmSymbol> export,SchemeEnvironment internal){
		this.name=name;
		this.export=export;
		this.internal=internal;
		LibraryManager.addLibrary(this);
	}
	/**
	 * Get the name
	 * @return
	 */
	public ScmPair getName(){
		return name;
	}
	/**
	 * Get the exported names
	 * @return
	 */
	public Set<ScmSymbol> getExportSet(){
		return export.keySet();
	}
	/**
	 * Get the export name mapping
	 * @return
	 */
	public HashMap<ScmSymbol,ScmSymbol> getExportMap(){
		return export;
	}
	/**
	 * Export the library to a environment
	 * @param env
	 */
	public void exportTo(Environment env){
		export.forEach((ex,im)->env.add(ex,internal.get(im)));
	}
	/**
	 * Export some names in the library to a environment
	 * @param env
	 * @param importset
	 */
	public void exportTo(Environment env,HashMap<ScmSymbol,ScmSymbol> importset){
		importset.forEach((ex,im)->env.add(im,internal.get(export.get(ex))));
	}
	/**
	 * Get the internal environment
	 * @return
	 */
	public SchemeEnvironment getInternalEnvironment(){
		return internal;
	}
	@Override
	public String toExternalRepresentation(){
		return name.toString();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
}