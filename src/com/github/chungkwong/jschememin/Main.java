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
import java.io.*;
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Main {
	/**
	 * Command line arguments given to JSchemeMin
	 */
	public static ScmPairOrNil COMMAND_LINE;
	/**
	 * Entrance to the interpreter
	 * @param args the command line arguments
	 */
	public static void main(String[] args) {
		COMMAND_LINE=(ScmPairOrNil)Arrays.stream(args).map((arg)->new ScmString(arg)).collect(ScmList.COLLECTOR);
		if(args.length>=1&&args[0].equals("-d")){
			Debugger.main(args);
			return;
		}
		System.out.println("JSchemeMin REPL");
		Evaluator eval=new Evaluator(true);
		System.out.print("> ");
		System.out.flush();
		Parser parser=new Parser(new Lex(new InputStreamReader(System.in)));
		while(true){
			try{
				ScmObject d=parser.nextDatum();
				if(d==null)
					return;
				System.out.println("=> "+eval.eval(d));
			}catch(RuntimeException ex){
				ex.printStackTrace();
			}
			System.out.print("> ");
			System.out.flush();
		}
	}
	/**
	 * Find a find
	 * @param path the path to the file
	 * @return
	 */
	public static File resolveFile(String path){
		return new File(path).getAbsoluteFile();
	}
}