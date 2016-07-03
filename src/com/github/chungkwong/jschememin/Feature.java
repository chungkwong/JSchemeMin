/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.chungkwong.jschememin;
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
		FEATURES.add("ieee-float");
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
}
