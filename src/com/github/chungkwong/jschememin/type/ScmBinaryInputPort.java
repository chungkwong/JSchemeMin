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
import java.math.*;
import java.util.*;
/**
 * Represents the type binary input port in Scheme
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmBinaryInputPort extends ScmPort{
	private final PushbackInputStream in;
	/**
	 * Wrap a InputStream
	 * @param in
	 */
	public ScmBinaryInputPort(PushbackInputStream in){
		this.in=in;
	}
	/**
	 * Wrap a InputStream
	 * @param in
	 */
	public ScmBinaryInputPort(InputStream in){
		this(new PushbackInputStream(in));
	}
	/**
	 * Construct a port to read from a file
	 * @param file the file name
	 */
	public ScmBinaryInputPort(String file){
		try{
			this.in=new PushbackInputStream(new FileInputStream(Main.resolveFile(file)));
		}catch(FileNotFoundException ex){
			throw ScmError.toException(new ScmError(new ScmString(ex.getLocalizedMessage()),ScmNil.NIL,ScmError.ErrorType.FILE));
		}
	}
	/**
	 * Corresponding to the procedure read-u8 in Scheme 
	 * @return
	 * @throws IOException
	 */
	public ScmObject readByte() throws IOException{
		int b=in.read();
		return b==-1?ScmEndOfFileObject.INSTANCE:new ScmInteger(BigInteger.valueOf(b));
	}
	/**
	 * Corresponding to the procedure peek-u8 in Scheme 
	 * @return
	 * @throws IOException
	 */
	public ScmObject peekByte() throws IOException{
		int b=in.read();
		if(b==-1)
			return ScmEndOfFileObject.INSTANCE;
		else{
			in.unread(b);
			return new ScmInteger(BigInteger.valueOf(b));
		}
	}
	/**
	 * Corresponding to the procedure read-bytevector in Scheme 
	 * @param max
	 * @return
	 * @throws IOException
	 */
	public ScmObject readBytevector(ScmInteger max) throws IOException{
		int len=max.getValue().intValueExact();
		byte[] buf=new byte[len];
		len=in.read(buf);
		return len==-1?ScmEndOfFileObject.INSTANCE:new ScmByteVector(Arrays.copyOf(buf,len));
	}
	/**
	 * Corresponding to the procedure read-bytevector in Scheme 
	 * @param vector
	 * @param start
	 * @param end
	 * @return
	 * @throws IOException
	 */
	public ScmObject readBytevector(ScmByteVector vector,ScmInteger start,ScmInteger end) throws IOException{
		int offset=start.getValue().intValueExact();
		int len=end.getValue().intValueExact()-offset;
		len=in.read(vector.getByteArray(),offset,len);
		return len==-1?ScmEndOfFileObject.INSTANCE:new ScmInteger(BigInteger.valueOf(len));
	}
	/**
	 * Corresponding to the procedure u8-ready in Scheme 
	 * @return
	 * @throws IOException
	 */
	public ScmBoolean ready() throws IOException{
		return ScmBoolean.valueOf(in.available()>=1);
	}
	/**
	 * Corresponding to the procedure close-input-port in Scheme 
	 * @return
	 * @throws IOException
	 */
	@Override
	public ScmBinaryInputPort close()throws IOException{
		super.close();
		in.close();
		return this;
	}
	@Override
	public String toExternalRepresentation(){
		return in.toString();
	}
}