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
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class ScmBinaryOutputPort extends ScmPort{
	private final OutputStream out;
	public ScmBinaryOutputPort(OutputStream out){
		this.out=out;
	}
	public ScmBinaryOutputPort(String file) throws FileNotFoundException, UnsupportedEncodingException{
		this.out=new FileOutputStream(file);
	}
	public void writeByte(ScmInteger obj) throws IOException{
		int b=obj.getValue().intValueExact();
		if(b>=0&&b<256)
			out.write(b);
		else
			throw new RuntimeException("A byte is expected");
	}
	public void writeByteVector(ScmByteVector obj,ScmInteger start,ScmInteger end) throws IOException{
		int off=start.getValue().intValueExact();
		int len=end.getValue().intValueExact()-off;
		out.write(obj.getByteArray(),off,len);
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
}