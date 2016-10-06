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
package com.github.chungkwong.jschememin;
import com.github.chungkwong.jschememin.type.*;
import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.stream.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Debugger{
	private final IdentityHashMap<ScmPair,Object> breakPoints=new IdentityHashMap<>();
	private final IdentityHashMap<ScmPair,Object> caredExpressions=new IdentityHashMap<>();
	private final IdentityHashMap<ScmPair,Object> coveredExpressions=new IdentityHashMap<>();
	private final ArrayList<ScmPair> caredExpressionsList=new ArrayList<>();
	private final ScmObject expr;
	private final Environment env;
	private final Continuation cont=new Continuation();
	public Debugger(ScmObject expr,boolean repl){
		this(expr,new Environment(repl));
	}
	public Debugger(ScmObject expr,Environment env){
		this.env=env;
		this.expr=expr;
		cont.callInit(ExpressionEvaluator.INSTANCE,expr,env);
	}
	public ScmObject getCurrentExpression(){
		return null;
	}
	public boolean step(){
		if(cont.hasNext()){
			cont.evalNext();
			if(isCaredPoint())
				coveredExpressions.put((ScmPair)cont.getCurrentValue(),null);
			return true;
		}else{
			return false;
		}
	}
	public void stepOver(){
		int level=cont.getLevel();
		while(step()){
			if(isExitedLevel(level))
				break;
		}
	}
	public void stepIn(){
		int level=cont.getLevel();
		while(step()){
			if(isCaredPoint()||isExitedLevel(level))
				break;
		}
	}
	public void run(){
		while(step()){
			if(isBreakPoint())
				break;
		}
	}
	private boolean isExitedLevel(int level){
		return cont.hasNext()&&cont.getLevel()==level&&
				cont.getCurrentEvaluable() instanceof ExpressionEvaluator&&cont.getCurrentPointer()==null;
	}
	private boolean isBreakPoint(){
		return cont.hasNext()&&cont.getCurrentEvaluable()instanceof ExpressionEvaluator&&
				breakPoints.containsKey(cont.getCurrentValue());
	}
	private boolean isCaredPoint(){
		return cont.hasNext()&&cont.getCurrentEvaluable()instanceof ExpressionEvaluator&&
				caredExpressions.containsKey(cont.getCurrentValue());
	}
	private boolean isFinished(){
		return cont.hasNext();
	}
	public ScmObject eval(ScmObject obj){
		return new Evaluator(cont.getCurrentEnvironment()).eval(obj);
	}
	public ScmObject getValue(){
		return cont.getCurrentValue();
	}
	public void addBreakPoint(int index){
		breakPoints.put(caredExpressionsList.get(index),null);
	}
	public void removeBreakPoint(int index){
		breakPoints.remove(caredExpressionsList.get(index));
	}
	public boolean isCovered(int index){
		return coveredExpressions.containsKey(caredExpressionsList.get(index));
	}
	public void addCaredExpression(ScmPair expr){
		if(!caredExpressions.containsKey(expr)){
			caredExpressions.put(expr,null);
			caredExpressionsList.add(expr);
			if(expr.getCar()instanceof ScmPair)
				addCaredExpression((ScmPair)expr.getCar());
			if(expr.getCdr()instanceof ScmPair)
				addCaredExpression((ScmPair)expr.getCdr());
		}
	}
	public List<ScmPair> getCaredExpressionsList(){
		return Collections.unmodifiableList(caredExpressionsList);
	}
	public static void main(String[] args) {
		System.out.println("JScmemeMin Debugger");
		System.out.println("Enter a program here:");
		Scanner in=new Scanner(System.in);
		Parser parser=new Parser(in.nextLine());
		ScmObject datum=parser.nextDatum();
		System.out.println("Enter a command(type \"help\" to show all commands):");
		Debugger debugger=new Debugger(datum,true);
		if(datum instanceof ScmPair)
			debugger.addCaredExpression((ScmPair)datum);
		System.out.print("> ");
		System.out.flush();
		while(in.hasNextLine()){
			String command=in.nextLine();
			if(command.equals("step in")){
				debugger.stepIn();
				showCurrentValues(debugger);
			}else if(command.equals("step over")){
				debugger.stepOver();
				showCurrentValues(debugger);
			}else if(command.equals("run")){
				debugger.run();
				showCurrentValues(debugger);
			}else if(command.equals("step")){
				debugger.step();
				showCurrentValues(debugger);
			}else if(command.equals("quit")){
				break;
			}else if(command.startsWith("eval")){
				System.out.println(debugger.eval(new Parser(command.substring(4)).nextDatum()));
			}else if(command.startsWith("break add")){
				debugger.addBreakPoint(Integer.parseInt(command.substring(9).trim()));
			}else if(command.startsWith("break remove")){
				debugger.removeBreakPoint(Integer.parseInt(command.substring(12).trim()));
			}else if(command.equals("stack")){
				System.out.println(debugger.cont);
			}else if(command.equals("help")){
				showHelp();
			}else if(command.equals("list")){
				showExpressionIndex(debugger);
			}
			System.out.print("> ");
			System.out.flush();
		}
	}
	private static void showExpressionIndex(Debugger debugger){
		List<ScmPair> list=debugger.caredExpressionsList;
		for(int i=0;i<list.size();i++)
			System.out.println(i+"\t"+list.get(i)+"\t"+debugger.isCovered(i));
	}
	private static void showCurrentValues(Debugger debugger){
		System.out.println(ScmList.asStream(debugger.cont.getCurrentValueRaw()).map((o)->o.toString()).collect(Collectors.joining(" ")));
		if(!debugger.cont.hasNext())
			System.out.println("Execution finished");
	}
	private static void showHelp(){
		try(BufferedReader in=new BufferedReader(new InputStreamReader(Debugger.class.getResourceAsStream("HELP"),"UTF-8"))){
			String line;
			while((line=in.readLine())!=null)
				System.out.println(line);
		}catch(IOException ex){
			Logger.getLogger(Debugger.class.getName()).log(Level.SEVERE,null,ex);
		}
	}
}