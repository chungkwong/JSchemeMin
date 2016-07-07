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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class DefineRecordType extends PrimitiveType{
	public static final DefineRecordType INSTANCE=new DefineRecordType();
	public DefineRecordType(){
		super(new ScmSymbol("define-record-type"));
	}
	@Override
	public void call(Environment env,Continuation cont,Object pointer,ScmObject param){
		ScmSymbol name=(ScmSymbol)((ScmPair)param).getCar();
		param=((ScmPair)param).getCdr();
		ScmPair constructor=(ScmPair)((ScmPair)param).getCar();
		ScmRecordType type=new ScmRecordType(name,constructor.getCdr());
		env.add((ScmSymbol)constructor.getCar(),type.getConstractor());
		param=((ScmPair)param).getCdr();
		env.add((ScmSymbol)((ScmPair)param).getCar(),type.getPredicate());
		param=((ScmPair)param).getCdr();
		while(param instanceof ScmPair){
			ScmPair field=(ScmPair)((ScmPair)param).getCar();
			ScmSymbol fieldname=(ScmSymbol)field.getCar();
			field=(ScmPair)field.getCdr();
			env.add((ScmSymbol)field.getCar(),type.getAccessor(fieldname));
			if(field.getCdr() instanceof ScmPair){
				env.add((ScmSymbol)((ScmPair)field.getCdr()).getCar(),type.getModifier(fieldname));
			}
			param=((ScmPair)param).getCdr();
		}
	}
}