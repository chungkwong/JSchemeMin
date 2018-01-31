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
package com.github.chungkwong.jschememin.type;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
class ObjectPair{
	private final Object car,cdr;
	private ObjectPair(Object car,Object cdr){
		this.car=car;
		this.cdr=cdr;
	}
	@Override
	public boolean equals(Object obj){
		return obj instanceof ObjectPair&&((ObjectPair)obj).car==car&&((ObjectPair)obj).cdr==cdr;
	}
	@Override
	public int hashCode(){
		int hash=7;
		hash=23*hash+System.identityHashCode(car);
		hash=23*hash+System.identityHashCode(cdr);
		return hash;
	}
	static boolean equals(Object a,Object b,HashSet<ObjectPair> found){
		if(a instanceof ScmPair&&b instanceof ScmPair){
			ScmPair c=(ScmPair)a,d=(ScmPair)b;
			ObjectPair pair=new ObjectPair(a,b);
			if(found.contains(pair))
				return true;
			else{
				found.add(pair);
				return equals(c.getCar(),d.getCar(),found)&&equals(c.getCdr(),d.getCdr(),found);
			}
		}else if(a instanceof ScmVector&& b instanceof ScmVector){
			ScmVector c=(ScmVector)a,d=(ScmVector)b;
			ObjectPair pair=new ObjectPair(a,b);
			if(found.contains(pair))
				return true;
			else{
				found.add(pair);
				if(c.getLength()==d.getLength())
					for(int i=0;i<c.getLength();i++)
						if(!equals(c.get(i),d.get(i),found))
							return false;
				return true;
			}
		}else{
			return !(a instanceof ScmPair)&&!(b instanceof ScmPair)&&
					!(a instanceof ScmVector)&&!(b instanceof ScmVector)
					&&Objects.equals(a,b);
		}
	}
}
