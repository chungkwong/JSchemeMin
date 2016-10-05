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
import java.util.*;
/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class Debugger{
	private final IdentityHashMap<ScmPair,Object> breakPoints=new IdentityHashMap<>();
	private final IdentityHashMap<ScmPair,Object> caredExpessions=new IdentityHashMap<>();
	private final Environment env;
	private final Continuation cont=new Continuation();
	public Debugger(ScmObject expr,boolean repl){
		this(expr,new Environment(repl));
	}
	public Debugger(ScmObject expr,Environment env){
		this.env=env;
		cont.callInit(ExpressionEvaluator.INSTANCE,expr,env);
	}
	public ScmObject getCurrentExpression(){
		return null;
	}
	public void stepOver(){
		int level=cont.getLevel();
		while(cont.hasNext()){
			cont.evalNext();
			if(isExitedLevel(level))
				break;
		}
	}
	public void stepIn(){
		while(cont.hasNext()){
			cont.evalNext();
			if(isCaredPoint())
				break;
		}
	}
	public void run(){
		while(cont.hasNext()){
			cont.evalNext();
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
				caredExpessions.containsKey(cont.getCurrentValue());
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
	public void addBreakPoint(ScmPair expr){
		breakPoints.put(expr,null);
	}
	public void removeBreakPoint(ScmPair expr){
		breakPoints.remove(expr);
	}
	public void addCaredExpression(ScmPair expr){
		if(!caredExpessions.containsKey(expr)){
			caredExpessions.put(expr,null);
			if(expr.getCar()instanceof ScmPair)
				addCaredExpression((ScmPair)expr.getCar());
			if(expr.getCdr()instanceof ScmPair)
				addCaredExpression((ScmPair)expr.getCdr());
		}
	}
	public void removeCaredExpression(ScmPair expr){
		caredExpessions.remove(expr);
	}
	public static void main(String[] args) {
		Main.COMMAND_LINE=(ScmPairOrNil)Arrays.stream(args).map((arg)->new ScmString(arg)).collect(ScmList.COLLECTOR);
		Scanner in=new Scanner(System.in);
		Parser parser=new Parser(in.nextLine());
		ScmObject datum=parser.nextDatum();
		Debugger debugger=new Debugger(datum,true);
		if(datum instanceof ScmPair)
			debugger.addCaredExpression((ScmPair)datum);
		while(in.hasNextLine()){
			String command=in.nextLine();
			if(command.equals("step in")){
				debugger.stepIn();
			}else if(command.equals("step over")){
				debugger.stepOver();
			}else if(command.equals("run")){
				debugger.run();
			}else if(command.equals("quit")){
				break;
			}else if(command.startsWith("eval")){
				System.out.println(debugger.eval(new Parser(command.substring(4)).nextDatum()));
			}
			System.out.println(debugger.getValue());
		}
	}
}