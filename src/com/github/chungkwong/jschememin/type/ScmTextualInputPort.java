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
/**
 * Represents the type Textual input port in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmTextualInputPort extends ScmPort{
	private final PushbackReader in;
	/**
	 * Wrap a Reader
	 * @param in the Reader
	 */
	public ScmTextualInputPort(PushbackReader in){
		this.in=in;
	}
	/**
	 * Wrap a Reader
	 * @param in the Reader
	 */
	public ScmTextualInputPort(Reader in){
		this(new PushbackReader(in,2));
	}
	/**
	 * Construct a port to read from a text file
	 * @param file the filename
	 */
	public ScmTextualInputPort(String file){
		try{
			this.in=new PushbackReader(new InputStreamReader(new FileInputStream(Main.resolveFile(file)),"UTF-8"),2);
		}catch(UnsupportedEncodingException|FileNotFoundException ex){
			throw ScmError.toException(new ScmError(new ScmString(ex.getLocalizedMessage()),ScmNil.NIL,ScmError.ErrorType.FILE));
		}
	}
	/**
	 * Corresponding to the read procedure in scheme
	 * @return the object read
	 */
	public ScmObject read(){
		ScmObject datum=new Parser(new Lex(in)).nextDatum();
		return datum!=null?datum:ScmEndOfFileObject.INSTANCE;
	}
	/**
	 * Corresponding to the read-char procedure in scheme
	 * @return the object read
	 * @throws IOException
	 */
	public ScmObject readCharacter() throws IOException{
		int high=in.read();
		if(high==-1)
			return ScmEndOfFileObject.INSTANCE;
		else if(Character.isHighSurrogate((char)high)){
			int low=in.read();
			return new ScmCharacter(Character.toCodePoint((char)high,(char)low));
		}else
			return new ScmCharacter(high);
	}
	/**
	 * Corresponding to the peek-char procedure in scheme
	 * @return the object read
	 * @throws IOException
	 */
	public ScmObject peekCharacter() throws IOException{
		int high=in.read();
		if(high==-1)
			return ScmEndOfFileObject.INSTANCE;
		else if(Character.isHighSurrogate((char)high)){
			int low=in.read();
			in.unread(low);
			in.unread(high);
			return new ScmCharacter(Character.toCodePoint((char)high,(char)low));
		}else{
			in.unread(high);
			return new ScmCharacter(high);
		}
	}
	private int readCodepoint() throws IOException{
		int high=in.read();
		if(high==-1)
			return -1;
		else if(Character.isHighSurrogate((char)high)){
			int low=in.read();
			return Character.toCodePoint((char)high,(char)low);
		}else
			return high;
	}
	/**
	 * Corresponding to the read-string procedure in scheme
	 * @param max the maximal number of characters to be read
	 * @return the object read
	 * @throws IOException
	 */
	public ScmObject readString(ScmInteger max) throws IOException{
		int len=max.getValue().intValueExact();
		StringBuilder buf=new StringBuilder(len);
		int c;
		while(--len>=0&&(c=readCodepoint())!=-1)
			buf.appendCodePoint(c);
		return buf.length()==0?ScmEndOfFileObject.INSTANCE:new ScmString(buf.toString());
	}
	/**
	 * Corresponding to the read-line procedure in scheme
	 * @return the object read
	 * @throws IOException
	 */
	public ScmObject readLine() throws IOException{
		String line=new BufferedReader(in).readLine();
		return line!=null?new ScmString(line):ScmEndOfFileObject.INSTANCE;
	}
	/**
	 * Corresponding to the char-ready? procedure in scheme
	 * @return if the next character is surely ready
	 * @throws IOException
	 */
	public ScmBoolean ready() throws IOException{
		return ScmBoolean.valueOf(in.ready());
	}
	/**
	 * Corresponding to the close-input-port procedure in scheme
	 * @return this
	 * @throws IOException
	 */
	@Override
	public ScmTextualInputPort close()throws IOException{
		super.close();
		in.close();
		return this;
	}
	@Override
	public String toExternalRepresentation(){
		return in.toString();
	}

	/**
	 * Get the underlying Reader
	 * @return the Reader
	 */
	public PushbackReader getReader(){
		return in;
	}
}