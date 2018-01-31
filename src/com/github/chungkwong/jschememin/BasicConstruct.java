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
/**
 * The base class for primitives
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class BasicConstruct extends Evaluable{
	private final ScmSymbol keyword;
	/**
	 * Construct a BasicConstruct
	 * @param keyword
	 */
	protected BasicConstruct(ScmSymbol keyword){
		this.keyword=keyword;
	}
	/**
	 * Get the keyword
	 * @return
	 */
	public ScmSymbol getKeyword(){
		return keyword;
	}
	@Override
	public String toExternalRepresentation(){
		return keyword.getValue();
	}

}
