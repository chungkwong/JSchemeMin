/*
 * Copyright (C) 2016 Chan Chung Kwong
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 */
package com.github.chungkwong.jschememin;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Main {
	public static ScmPairOrNil COMMAND_LINE;
	/**
	 * @param args the command line arguments
	 */
	public static void main(String[] args) {
		ScmListBuilder buf=new ScmListBuilder();
		Arrays.stream(args).map((arg)->new ScmString(arg)).forEach((arg)->buf.add(arg));
		COMMAND_LINE=buf.toList();
	}

}