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
import com.github.chungkwong.jschememin.*;
import java.io.*;
import java.util.*;
import java.util.stream.*;
/**
 * Represents the type textual output port in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmTextualOutputPort extends ScmPort{
	private static final String LINE_SEPARATOR=System.getProperty("line.separator");
	private final Writer out;
	/**
	 * Wrap a Writer
	 * @param out the Writer
	 */
	public ScmTextualOutputPort(Writer out){
		this.out=out;
	}
	/**
	 * Construct a port to write to a file
	 * @param file the file name
	 */
	public ScmTextualOutputPort(String file){
		try{
			this.out=new OutputStreamWriter(new FileOutputStream(Main.resolveFile(file)),"UTF-8");
		}catch(UnsupportedEncodingException|FileNotFoundException ex){
			throw ScmError.toException(new ScmError(new ScmString(ex.getLocalizedMessage()),ScmNil.NIL,ScmError.ErrorType.FILE));
		}
	}
	/**
	 * Corresponding to the write-shared procedure in scheme
	 * @param obj the object to be written
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort writeShared(ScmObject obj) throws IOException{
		out.write(obj.toExternalRepresentation());
		return this;
	}
	/**
	 * Corresponding to the write-simple procedure in scheme
	 * @param obj the object to be written
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort writeSimple(ScmObject obj) throws IOException{
		out.write(toSimpleRepresentation(obj));
		return this;
	}
	/**
	 * Corresponding to the write procedure in scheme
	 * @param obj the object to be written
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort write(ScmObject obj) throws IOException{
		out.write(hasCycle(obj,new IdentityHashMap<>())?obj.toExternalRepresentation():toSimpleRepresentation(obj));
		return this;
	}
	/**
	 * Corresponding to the write-char procedure in scheme
	 * @param obj the object to be written
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort writeCharacter(ScmCharacter obj) throws IOException{
		out.write(new String(new int[]{obj.getCodePoint()},0,1));
		return this;
	}
	/**
	 * Corresponding to the write-string procedure in scheme
	 * @param obj the object to be written
	 * @param start the index of the first character to be written
	 * @param end the index after the first character to be written
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort writeString(ScmString obj,int start,int end) throws IOException{
		int off=start;
		int len=end-off;
		out.write(obj.getValue(),off,len);
		return this;
	}
	/**
	 * Corresponding to the display procedure in scheme
	 * @param obj the object to be written
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort display(ScmObject obj) throws IOException{
		if(obj instanceof ScmString)
			out.write(((ScmString)obj).getValue());
		else if(obj instanceof ScmCharacter)
			out.write(new String(new int[]{((ScmCharacter)obj).getCodePoint()},0,1));
		else if(obj instanceof ScmSymbol)
			out.write(((ScmSymbol)obj).getValue());
		else
			out.write(obj.toExternalRepresentation());
		return this;
	}
	/**
	 * Corresponding to the get-output-string procedure in scheme
	 * @return this
	 */
	public String getString(){
		if(out instanceof StringWriter)
			return ((StringWriter)out).toString();
		else
			throw new RuntimeException();
	}
	/**
	 * Corresponding to the newline procedure in scheme
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort newline() throws IOException{
		out.write(LINE_SEPARATOR);
		return this;
	}
	/**
	 * Corresponding to the flush-output-port procedure in scheme
	 * @return this
	 * @throws IOException
	 */
	public ScmTextualOutputPort flush()throws IOException{
		out.flush();
		return this;
	}
	/**
	 * Corresponding to the close-output-port procedure in scheme
	 * @return this
	 * @throws IOException
	 */
	@Override
	public ScmTextualOutputPort close()throws IOException{
		super.close();
		out.close();
		return this;
	}
	@Override
	public String toExternalRepresentation(){
		return out.toString();
	}
	private static String toSimpleRepresentation(ScmObject obj){
		if(obj instanceof ScmPair){
			return "("+toSimpleRepresentation(((ScmPair)obj).getCar())+" . "+toSimpleRepresentation(((ScmPair)obj).getCdr())+")";
		}else if(obj instanceof ScmVector){
			return ((ScmVector)obj).stream().map((e)->toSimpleRepresentation(e)).collect(Collectors.joining(" ","#(",")"));
		}else
			return obj.toExternalRepresentation();
	}
	private static boolean hasCycle(ScmObject obj,IdentityHashMap<ScmObject,Object> found){
		if(found.containsKey(obj))
			return true;
		if(obj instanceof ScmPair){
			found.put(obj,null);
			return (hasCycle(((ScmPair)obj).getCar(),found)||hasCycle(((ScmPair)obj).getCdr(),found))&found.remove(obj)==null;
		}else if(obj instanceof ScmVector){
			found.put(obj,null);
			return ((ScmVector)obj).stream().anyMatch((o)->hasCycle(o,found))&found.remove(obj)==null;
		}else
			return false;
	}
}