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
package com.github.chungkwong.jschememin.primitive;
import com.github.chungkwong.jschememin.*;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 * Correspoding to the primitive import in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Import extends BasicConstruct implements Primitive{
	public static final Import INSTANCE=new Import();
	private static final ScmSymbol PREFIX=new ScmSymbol("prefix");
	private static final ScmSymbol RENAME=new ScmSymbol("rename");
	private static final ScmSymbol EXCEPT=new ScmSymbol("except");
	private static final ScmSymbol ONLY=new ScmSymbol("only");
	private Import(){
		super(new ScmSymbol("import"));
	}
	@Override
	public void call(SchemeEnvironment env,Continuation cont,Object pointer,ScmPairOrNil list){
		importLibraries(env,list);
		cont.ret(new ScmSymbol("ok"));
	}
	public SchemeEnvironment importLibraries(SchemeEnvironment env,ScmObject list){
		ScmList.forEach(list,(o)->importLibrary(env,(ScmPair)o));
		return env;
	}
	private void importLibrary(SchemeEnvironment env,ScmPair spec){
		ImportSet set=getImportSet(spec);
		set.lib.exportTo(env,set.ex2im);
	}
	private ImportSet getImportSet(ScmPair list){
		if(list.getCdr() instanceof ScmPair&&((ScmPair)list.getCdr()).getCar() instanceof ScmPair){
			ScmObject car=list.getCar();
			ImportSet set=getImportSet((ScmPair)list.getCadr());
			list=(ScmPair)list.getCddr();
			if(car.equals(PREFIX)){
				ScmSymbol prefix=(ScmSymbol)list.getCar();
				for(Map.Entry<ScmSymbol,ScmSymbol> entry:set.ex2im.entrySet()){
					entry.setValue(new ScmSymbol(prefix.getValue()+entry.getValue().getValue()));
				}
			}else if(car.equals(RENAME)){
				HashMap<ScmSymbol,ScmSymbol> rename=new HashMap<>();
				ScmList.forEach(list,(c)->rename.put((ScmSymbol)((ScmPair)c).getCar(),(ScmSymbol)((ScmPair)c).getCadr()));
				for(Map.Entry<ScmSymbol,ScmSymbol> entry:set.ex2im.entrySet())
					if(rename.containsKey(entry.getValue()))
						entry.setValue(rename.get(entry.getValue()));
			}else if(car.equals(EXCEPT)){
				HashSet<ScmSymbol> remain=new HashSet<>();
				ScmList.forEach(list,(c)->remain.add((ScmSymbol)c));
				Iterator<Map.Entry<ScmSymbol,ScmSymbol>> iter=set.ex2im.entrySet().iterator();
				while(iter.hasNext())
					if(remain.contains(iter.next().getValue()))
						iter.remove();
			}else if(car.equals(ONLY)){
				HashSet<ScmSymbol> remain=new HashSet<>();
				ScmList.forEach(list,(c)->remain.add((ScmSymbol)c));
				Iterator<Map.Entry<ScmSymbol,ScmSymbol>> iter=set.ex2im.entrySet().iterator();
				while(iter.hasNext())
					if(!remain.contains(iter.next().getValue()))
						iter.remove();
			}
			return set;
		}else
			return new ImportSet(list);
	}
	static class ImportSet{
		final Library lib;
		final HashMap<ScmSymbol,ScmSymbol> ex2im=new HashMap<>();
		public ImportSet(ScmPair libName){
			lib=LibraryManager.getLibrary(libName);
			lib.getExportSet().stream().forEach((exportName)->{
				ex2im.put(exportName,exportName);
			});
		}
	}
}
