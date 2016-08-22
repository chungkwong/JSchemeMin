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
import java.util.function.*;
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
		return (ScmPairOrNil)buf.toList();
	}
	static final NativeProcedure chainComparator(BiPredicate<ScmObject,ScmObject> comparator){
		return (list)->{
			ScmObject prev=car(list);
			list=cdr(list);
			while(list instanceof ScmPair){
				ScmObject curr=car(list);
				if(!comparator.test(prev,curr))
					return ScmBoolean.FALSE;
				prev=curr;
				list=cdr(list);
			}
			return ScmBoolean.TRUE;
		};
	}
	static final NativeProcedure reducer(BiFunction<ScmObject,ScmObject,ScmObject> f){
		return (list)->{
			ScmObject result=car(list);
			list=cdr(list);
			while(list instanceof ScmPair){
				result=f.apply(result,car(list));
				list=cdr(list);
			}
			return result;
		};
	}
	static final NativeProcedure reducer(BiFunction<ScmObject,ScmObject,ScmObject> f,Function<ScmObject,ScmObject> g){
		return (list)->{
			ScmObject result=car(list);
			list=cdr(list);
			if(list instanceof  ScmNil)
				return g.apply(result);
			while(list instanceof ScmPair){
				result=f.apply(result,car(list));
				list=cdr(list);
			}
			return result;
		};
	}
	static final NativeProcedure reducer(BiFunction<ScmObject,ScmObject,ScmObject> f,ScmObject id){
		return (list)->{
			ScmObject result=id;
			while(list instanceof ScmPair){
				result=f.apply(result,car(list));
				list=cdr(list);
			}
			return result;
		};
	}
	static final NativeProcedure correctExactness(NativeProcedure proc){
		return (list)->{
			ScmComplex result=(ScmComplex)proc.call(list);
			if(ScmList.asStream((ScmPairOrNil)list).allMatch((o)->((ScmComplex)o).isExact()))
				return result;
			else
				return result.toInExact();
		};
	}
	static final NativeProcedure correctExactnessMulti(NativeProcedure proc){
		return (list)->{
			ScmPairOrNil result=(ScmPairOrNil)proc.call(list);
			if(ScmList.asStream((ScmPairOrNil)list).allMatch((o)->((ScmComplex)o).isExact()))
				return result;
			else{
				ScmListBuilder buf=new ScmListBuilder();
				ScmList.forEach(result,(o)->buf.add(((ScmComplex)o).toInExact()));
				return buf.toList();
			}
		};
	}
	static final void emergencyExit(ScmObject obj){
		int status;
		if(obj==ScmBoolean.TRUE)
			status=0;
		else if(obj==ScmBoolean.FALSE)
			status=1;
		else
			try{
				status=((ScmComplex)obj).intValueExact();
			}catch(RuntimeException ex){
				status=1;
			}
		System.exit(status);
	}
	static final void exit(ScmObject obj){
		emergencyExit(obj);//TODO add after wind
	}
	static final Environment getInteractiveEnvironment(){
		return new Environment(true);
	}
	static final String toRadixPrefix(int radix){
		switch(radix){
			case 2:return "#b";
			case 8:return "#o";
			case 10:return "#d";
			case 16:return "#x";
			default:throw new RuntimeException();
		}
	}
}
