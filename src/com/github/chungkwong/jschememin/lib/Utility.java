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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Utility{
	static final ScmObject car(ScmObject o){
		return ((ScmPair)o).getCar();
	}
	static final ScmObject cdr(ScmObject o){
		return ((ScmPair)o).getCdr();
	}
	static final ScmObject cadr(ScmObject o){
		return ((ScmPair)o).getCadr();
	}
	static final ScmObject caddr(ScmObject o){
		return ((ScmPair)((ScmPair)o).getCddr()).getCar();
	}
	static final ScmObject cadddr(ScmObject o){
		return ((ScmPair)((ScmPair)o).getCddr()).getCadr();
	}
	static final ScmObject caddddr(ScmObject o){
		return ((ScmPair)((ScmPair)((ScmPair)o).getCddr()).getCddr()).getCar();
	}
	static final ScmObject getEnvironmentVariable(ScmString key){
		String value=System.getenv(key.getValue());
		return value==null?ScmBoolean.FALSE:new ScmString(value);
	}
	static final ScmPairOrNil getEnvironmentVariables(){
		ScmListBuilder buf=new ScmListBuilder();
		System.getenv().forEach((key,value)->buf.add(new ScmPair(new ScmString(key),new ScmString(value))));
		return buf.toList();
	}
}
