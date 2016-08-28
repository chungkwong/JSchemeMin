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
import java.io.*;
import java.util.*;
import java.util.stream.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmTextualOutputPort extends ScmPort{
	public static final String LINE_SEPARATOR=System.getProperty("line.separator");
	private final Writer out;
	public ScmTextualOutputPort(Writer out){
		this.out=out;
	}
	public ScmTextualOutputPort(String file) throws FileNotFoundException, UnsupportedEncodingException{
		this.out=new OutputStreamWriter(new FileOutputStream(file),"UTF-8");
	}
	public ScmTextualOutputPort writeShared(ScmObject obj) throws IOException{
		out.write(obj.toExternalRepresentation());
		return this;
	}
	public ScmTextualOutputPort writeSimple(ScmObject obj) throws IOException{
		out.write(toSimpleRepresentation(obj));
		return this;
	}
	public ScmTextualOutputPort write(ScmObject obj) throws IOException{
		out.write(hasCycle(obj,new HashSet<>())?obj.toExternalRepresentation():toSimpleRepresentation(obj));
		return this;
	}
	public ScmTextualOutputPort writeCharacter(ScmCharacter obj) throws IOException{
		out.write(new String(new int[]{obj.getCodePoint()},0,1));
		return this;
	}
	public ScmTextualOutputPort writeString(ScmString obj,int start,int end) throws IOException{
		int off=start;
		int len=end-off;
		out.write(obj.getValue(),off,len);
		return this;
	}
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
	public String getString(){
		if(out instanceof StringWriter)
			return ((StringWriter)out).toString();
		else
			throw new RuntimeException();
	}
	public ScmTextualOutputPort newline() throws IOException{
		out.write(LINE_SEPARATOR);
		return this;
	}
	public ScmTextualOutputPort flush()throws IOException{
		out.flush();
		return this;
	}
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
	public static String toSimpleRepresentation(ScmObject obj){
		if(obj instanceof ScmPair){
			return "("+toSimpleRepresentation(((ScmPair)obj).getCar())+" . "+toSimpleRepresentation(((ScmPair)obj).getCdr())+")";
		}else if(obj instanceof ScmVector){
			return ((ScmVector)obj).stream().map((e)->toSimpleRepresentation(e)).collect(Collectors.joining(" ","#(",")"));
		}else
			return obj.toExternalRepresentation();
	}
	private static boolean hasCycle(ScmObject obj,HashSet<ScmObject> found){
		return true;//TODO
	}
}