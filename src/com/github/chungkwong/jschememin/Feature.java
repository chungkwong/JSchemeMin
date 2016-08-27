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
public class Feature{
	private static final HashSet<String> FEATURES=new HashSet<>();
	static{
		FEATURES.add("r7rs");
		FEATURES.add("exact-closed");
		FEATURES.add("exact-complex");
		//FEATURES.add("ieee-float");
		FEATURES.add("full-unicode");
		FEATURES.add("ratios");
		FEATURES.add("jvm");
		FEATURES.add("jschememin");
		FEATURES.add("jschememin-0.0.1");
		FEATURES.add(System.getProperty("os.arch"));
		FEATURES.add(System.getProperty("os.name"));
	}
	public static boolean contains(String feature){
		return FEATURES.contains(feature);
	}
	public static ScmPairOrNil getAll(){
		return ScmList.toList(FEATURES.stream().map((feature)->new ScmSymbol(feature)).toArray(ScmObject[]::new));
	}
}