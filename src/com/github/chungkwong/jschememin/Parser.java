/*
 * Copyright (C) 2015,2016 Chan Chung Kwong
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
 * Parser for R7RS 
 * @author kwong
 */
public final class Parser{
	private final Lex lex;
	private Stack<Level> stack=new Stack<>();
	private final HashMap<DatumLabel,ScmObject> datumRef=new HashMap<>();
	private final HashMap<DatumLabel,List<Backtrack>> datumBacktrack=new HashMap<>();
	private static final HashMap<String,ScmSymbol> ABBERTIATION=new HashMap<>();
	private static final ScmSymbol COMMENT=new ScmSymbol("#;");
	static{
		ABBERTIATION.put("\'",new ScmSymbol("quote"));
		ABBERTIATION.put("`",new ScmSymbol("quasiquote"));
		ABBERTIATION.put(",",new ScmSymbol("unquote"));
		ABBERTIATION.put(",@",new ScmSymbol("unquote-splicing"));
	}
	/**
	 * Create a parser to parse from tokens
	 * @param lex
	 */
	public Parser(Lex lex){
		this.lex=lex;
	}
	/**
	 * Create a parser to parse from String
	 * @param str
	 */
	public Parser(String str){
		this(new Lex(str));
	}
	/**
	 * Parse the next datum
	 * @return the datum or null if ended
	 */
	public ScmObject nextDatum(){
		boolean nonComment;
		stack.add(new ListLevel());
		do{//tail recursive optimization by hand
			nonComment=true;
			Object token=lex.nextToken();
			if(token==null){
				break;
			}
			if(token instanceof DatumLabelRef){
				if(datumRef.containsKey((DatumLabelRef)token)){
					nonComment=addDatum(datumRef.get(((DatumLabelRef)token)));
				}else if(datumBacktrack.containsKey((DatumLabelRef)token)){
					nonComment=addDatum((DatumLabelRef)token);
				}else{
					throw new SyntaxException("Invalid label");
				}
			}else if(token instanceof DatumLabelSrc){
				if(datumBacktrack.containsKey((DatumLabelSrc)token)||datumRef.containsKey((DatumLabelSrc)token)){
					throw new SyntaxException("Multiple declaration label");
				}
				datumBacktrack.put((DatumLabelSrc)token,new LinkedList<>());
				stack.push(new ListLevel((DatumLabelSrc)token));
			}else if(token instanceof ScmObject){
				nonComment=addDatum((ScmObject)token);
			}else if(token instanceof SimpleToken){
				String name=token.toString();
				if(ABBERTIATION.containsKey(name)){
					stack.push(new ListLevel(ABBERTIATION.get(name)));
				}else if(name.equals(")")){
					nonComment=addDatum(stack.pop().getContent());
				}else if(name.equals("(")){
					stack.push(new ListLevel());
				}else if(name.equals("#(")){
					stack.push(new VectorLevel());
				}else if(name.equals("#u8(")){
					stack.push(new ByteVectorLevel());
				}else if(name.equals("#;")){
					stack.push(new ListLevel(COMMENT));
				}else if(name.equals(".")){
					stack.peek().addDot();
				}else{
					assert false;
				}
			}else{
				assert false;
			}
		}while(stack.size()>1||!nonComment);
		ScmPairOrNil ret=(ScmPairOrNil)stack.pop().getContent();
		return ret instanceof ScmPair?((ScmPair)ret).getCar():null;
	}
	private boolean addDatum(ScmObject datum){
		while(stack.peek().add(datum)){
			ScmPair pair=(ScmPair)stack.pop().getContent();
			ScmObject car=pair.getCar();
			if(car instanceof DatumLabelSrc){
				datumRef.put((DatumLabelSrc)car,datum);
				datumBacktrack.remove((DatumLabelSrc)car).forEach((b)->b.fillIn(((ScmPair)pair.getCdr()).getCar()));
			}else if(car==COMMENT){
				return false;
			}else{
				datum=pair;
			}
		}
		return true;
	}
	/**
	 * Parse all the remaining datums
	 * @return the datums
	 */
	public ArrayList<ScmObject> getRemainingDatums(){
		ArrayList<ScmObject> datums=new ArrayList<>();
		ScmObject datum;
		while((datum=nextDatum())!=null){
			datums.add(datum);
		}
		return datums;
	}
	public static void main(String[] args) throws Exception{
		Scanner in=new Scanner(System.in);
		while(in.hasNextLine()){
			System.out.println(new Parser(in.nextLine()).getRemainingDatums());
		}
	}
	interface Level{
		boolean add(ScmObject obj);
		void addDot();
		ScmObject getContent();
	}
	class ListLevel implements Level{
		ScmPairOrNil first;
		Object end;
		boolean last, pop;
		public ListLevel(ScmObject car){
			//pop=last=true;
			pop=true;
			last=false;
			end=first=ScmList.toList(car);
		}
		public ListLevel(){
			pop=last=false;
			end=first=ScmNil.NIL;
		}
		@Override
		public boolean add(ScmObject obj){
			if(last){
				((ScmPair)end).setCdr(obj);
				if(obj instanceof DatumLabelRef)
					datumBacktrack.get((DatumLabelRef)obj).add((o)->((ScmPair)end).setCdr(o));
			}else{
				ScmPair pair=ScmList.toList(obj);
				if(first==ScmNil.NIL){
					end=first=pair;
				}else{
					((ScmPair)end).setCdr(pair);
					end=pair;
				}
				if(obj instanceof DatumLabelRef)
					datumBacktrack.get((DatumLabelRef)obj).add((o)->pair.setCar(o));
			}
			return pop;
		}
		@Override
		public void addDot(){
			last=true;
		}
		@Override
		public ScmObject getContent(){
			return first;
		}
	}
	class VectorLevel implements Level{
		final ArrayList<ScmObject> vector=new ArrayList<>();
		public VectorLevel(){
		}
		@Override
		public boolean add(ScmObject obj){
			vector.add(obj);
			if(obj instanceof DatumLabelRef){
				int index=vector.size()-1;
				datumBacktrack.get((DatumLabelRef)obj).add((o)->vector.set(index,o));
			}
			return false;
		}
		@Override
		public void addDot(){
			throw new SyntaxException("Dot in vector");
		}
		@Override
		public ScmObject getContent(){
			return new ScmVector(vector);
		}
	}
	class ByteVectorLevel implements Level{
		BitSet vector=new BitSet();
		int length=0;
		public ByteVectorLevel(){
		}
		@Override
		public boolean add(ScmObject obj){
			int b;
			try{
				b=((ScmInteger)obj).getValue().intValueExact();
			}catch(RuntimeException ex){
				throw new SyntaxException("A byte is Expected");
			}
			if(b>=0&&b<256){
				for(int i=0;i<8;i++)
					if((b&(1<<i))!=0){
						vector.set(length+i);
					}
				length+=8;
				return false;
			}else{
				throw new SyntaxException("A byte is Expected");
			}
		}
		@Override
		public void addDot(){
			throw new SyntaxException("Dot in bytevector");
		}
		@Override
		public ScmObject getContent(){
			return new ScmByteVector(Arrays.copyOf(vector.toByteArray(),length/8));
		}
	}
	interface Backtrack{
		void fillIn(ScmObject obj);
	}
}