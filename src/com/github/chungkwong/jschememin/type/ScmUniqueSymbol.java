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

/**
 * Represents symbols that is difference to all existing ones
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmUniqueSymbol extends ScmSymbol{
	private static int seq=0;
	private final ScmSymbol org;
	/**
	 * Construcr a unique symbol
	 * @param org the prefered name
	 */
	public ScmUniqueSymbol(ScmSymbol org){
		super(Integer.toString(seq++));
		this.org=toLiteral(org);
	}
	/**
	 * Get the prefered name
	 * @return
	 */
	public ScmSymbol getOrigin(){
		return org;
	}
	/**
	 * Get prefered name or itself
	 * @param id
	 * @return
	 */
	public static ScmSymbol toLiteral(ScmSymbol id){
		return id instanceof ScmUniqueSymbol?((ScmUniqueSymbol)id).getOrigin():id;
	}
	@Override
	public String toString(){
		return org.toString(); //To change body of generated methods, choose Tools | Templates.
	}
}