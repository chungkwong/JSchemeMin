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
 * Represents errors in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmError extends ScmObject{
	/**
	 * The type of error in Scheme
	 */
	public static enum ErrorType{
		READ,FILE,SYNTAX,JAVA,OTHER
	}
	private final ScmString message;
	private final ScmPairOrNil irritants;
	private final ErrorType type;
	/**
	 * Construct a error
	 * @param message the error message
	 * @param irritants something related to the errir
	 * @param type the type of the error
	 */
	public ScmError(ScmString message,ScmPairOrNil irritants,ErrorType type){
		this.message=message;
		this.irritants=irritants;
		this.type=type;
	}

	/**
	 * Get the error message
	 * @return
	 */
	public ScmString getErrorMessage(){
		return message;
	}
	/**
	 * Get something related to the error
	 * @return
	 */
	public ScmPairOrNil getIrritants(){
		return irritants;
	}
	/**
	 * Get the type of the error
	 * @return
	 */
	public ErrorType getType(){
		return type;
	}
	@Override
	public String toExternalRepresentation(){
		return new ScmPair(new ScmSymbol("error"),new ScmPair(message,irritants)).toExternalRepresentation();
	}
	@Override
	public boolean isSelfevaluating(){
		return true;
	}
	/**
	 * Wrap a Throwable
	 * @param t
	 * @return
	 */
	public static RuntimeException toRuntimeException(Throwable t){
		if(t instanceof ScmException)
			return (ScmException)t;
		return new ScmException(toScmObject(t));
	}
	/**
	 * Wrap a error object
	 * @param obj
	 * @return
	 */
	public static RuntimeException toException(ScmObject obj){
		if(obj instanceof ScmError&&((ScmError)obj).getType()==ErrorType.JAVA)
			return new ScmException(obj,(Throwable)((ScmJavaObject)ScmList.first(((ScmError)obj).getIrritants())).getJavaObject());
		else
			return new ScmException(obj);
	}
	/**
	 * Convert to a Scheme object
	 * @param obj
	 * @return
	 */
	public static ScmObject toScmObject(Throwable obj){
		//obj.printStackTrace();
		if(obj instanceof ScmException)
			return ((ScmException)obj).getObject();
		return new ScmError(new ScmString(obj.toString()),ScmList.toList(new ScmJavaObject(obj)),ErrorType.JAVA);
	}
	private static class ScmException extends RuntimeException{
		private final ScmObject obj;
		public ScmException(ScmObject obj){
			super();
			this.obj=obj;
		}
		public ScmException(ScmObject obj,Throwable cause){
			super(cause);
			this.obj=obj;
		}
		public ScmObject getObject(){
			return obj;
		}
		@Override
		public String getMessage(){
			return obj.toExternalRepresentation();
		}
	}
}