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
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmError extends ScmObject{
	public static enum ErrorType{
		READ,FILE,OTHER
	}
	private final ScmString message;
	private final ScmPairOrNil irritants;
	private final ErrorType type;
	public ScmError(ScmString message,ScmPairOrNil irritants,ErrorType type){
		this.message=message;
		this.irritants=irritants;
		this.type=type;
	}
	public ScmString getErrorMessage(){
		return message;
	}
	public ScmPairOrNil getIrritants(){
		return irritants;
	}
	public ErrorType getType(){
		return type;
	}
	@Override
	public String toExternalRepresentation(){
		return new ScmPair(new ScmSymbol("error"),new ScmPair(message,irritants)).toExternalRepresentation();
	}
	@Override
	public boolean isSelfevaluating(){
		return false;
	}
}
