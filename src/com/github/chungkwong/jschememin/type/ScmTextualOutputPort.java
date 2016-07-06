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
import java.util.stream.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmTextualOutputPort extends ScmPort{
	public static ScmTextualOutputPort OUT=new ScmTextualOutputPort(new OutputStreamWriter(System.out));
	public static ScmTextualOutputPort ERR=new ScmTextualOutputPort(new OutputStreamWriter(System.err));
	public static String LINE_SEPARATOR=System.getProperty("line.separator");
	private final Writer out;
	public ScmTextualOutputPort(Writer out){
		this.out=out;
	}
	public ScmTextualOutputPort(String file) throws FileNotFoundException, UnsupportedEncodingException{
		this.out=new OutputStreamWriter(new FileOutputStream(file),"UTF-8");
	}
	public void writeShared(ScmObject obj) throws IOException{
		out.write(obj.toExternalRepresentation());
	}
	public void writeSimple(ScmObject obj) throws IOException{
		out.write(toSimpleRepresentation(this));
	}
	public void write(ScmObject obj) throws IOException{
		out.write(obj.toExternalRepresentation());//TODO
	}
	public void writeCharacter(ScmCharacter obj) throws IOException{
		out.write(new String(new int[]{obj.getCodePoint()},0,1));
	}
	public void writeString(ScmString obj,ScmInteger start,ScmInteger end) throws IOException{
		int off=start.getValue().intValueExact();
		int len=end.getValue().intValueExact()-off;
		out.write(obj.getValue(),off,len);
	}
	public void display(ScmObject obj) throws IOException{
		if(obj instanceof ScmString)
			out.write(((ScmString)obj).getValue());
		else if(obj instanceof ScmCharacter)
			out.write(new String(new int[]{((ScmCharacter)obj).getCodePoint()},0,1));
		else
			out.write(obj.toExternalRepresentation());
	}
	public void newline() throws IOException{
		out.write(LINE_SEPARATOR);
	}
	public void flush()throws IOException{
		out.flush();
	}
	public void close()throws IOException{
		out.close();
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
}