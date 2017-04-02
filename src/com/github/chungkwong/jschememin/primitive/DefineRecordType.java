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
public class DefineRecordType extends BasicConstruct implements Primitive{
	public static final DefineRecordType INSTANCE=new DefineRecordType();
	public DefineRecordType(){
		super(new ScmSymbol("define-record-type"));
	}
	@Override
	public void call(SchemeEnvironment env,Continuation cont,Object pointer,ScmPairOrNil param){
		ScmSymbol name=(ScmSymbol)ScmList.first(param);
		param=(ScmPairOrNil)((ScmPair)param).getCdr();
		ScmPair constructor=(ScmPair)ScmList.first(param);
		ScmRecordType type=new ScmRecordType(name,constructor.getCdr());
		env.add((ScmSymbol)constructor.getCar(),type.getConstractor());
		param=(ScmPairOrNil)((ScmPair)param).getCdr();
		env.add((ScmSymbol)ScmList.first(param),type.getPredicate());
		param=(ScmPairOrNil)((ScmPair)param).getCdr();
		while(param instanceof ScmPair){
			ScmPair field=(ScmPair)ScmList.first(param);
			ScmSymbol fieldname=(ScmSymbol)field.getCar();
			field=(ScmPair)field.getCdr();
			env.add((ScmSymbol)field.getCar(),type.getAccessor(fieldname));
			if(field.getCdr() instanceof ScmPair){
				env.add((ScmSymbol)((ScmPair)field.getCdr()).getCar(),type.getModifier(fieldname));
			}
			param=(ScmPairOrNil)((ScmPair)param).getCdr();
		}
		cont.ret(type);
	}
}