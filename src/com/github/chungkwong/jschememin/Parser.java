package com.github.chungkwong.jschememin;
import com.github.chungkwong.jschememin.type.*;
import java.util.*;
public final class Parser implements TokenIterator{
	Lex lex;
	private Stack<Level> stack=new Stack<Level>();
	HashMap<String,Object> datumRef=new HashMap<String,Object>();
	static final HashMap<String,String> abbreviation=new HashMap<String,String>();
	static{
		abbreviation.put("\'","quote");
		abbreviation.put("`","quasiquote");
		abbreviation.put(",","unquote");
		abbreviation.put(",@","unquote-splicing");
		/*abbreviation.put("#’","syntax");
		abbreviation.put("#`","quasisyntax");
		abbreviation.put("#,","unsyntax");
		abbreviation.put("#,@","unsyntax-splicing");*/
	}
	public Parser(Lex lex){
		this.lex=lex;
	}
	public Parser(String str){
		this(new Lex(str));
	}
	public Object nextDatum(){
		boolean nonComment=true;
		stack.add(new ListLevel());
		do{//tail recursive optimization by hand
			Object token=lex.nextToken();
			if(token==null)
				break;
			if(token instanceof Boolean||token instanceof Character||token instanceof String||token instanceof Identifier
				||token instanceof Number||token instanceof Rational||token instanceof Tuple)
					nonComment=addDatum(token);
			else if(token instanceof Token){
				String name=token.toString();
				if(abbreviation.containsKey(name)){
					stack.push(new ListLevel(abbreviation.get(name)));
				}else if(name.equals(")")){
					nonComment=addDatum(stack.pop().getContent());
				}else if(name.equals("(")){
					stack.push(new ListLevel());
				}else if(name.equals("#(")){
					stack.push(new VectorLevel());
				}else if(name.equals("#u8(")){
					stack.push(new ByteVectorLevel());
				}else if(name.equals("#;")){
					stack.push(new ListLevel(token));
				}else if(name.equals(".")){
					stack.peek().addDot();
				}
			}else if(token instanceof DatumLabelRef){
				nonComment=addDatum(datumRef.get(((DatumLabelRef)token).getLabel()));
			}else if(token instanceof DatumLabelSrc){
				stack.push(new ListLevel(token));
			}
		}while(stack.size()>1||!nonComment);
		ScmPairOrNil ret=(ScmPairOrNil)stack.pop().getContent();
		return ret instanceof ScmPair?((ScmPair)ret).getCar():null;
	}
	private final boolean addDatum(Object datum){
		while(stack.peek().add(datum)){
			Object pair=stack.pop().getContent();
			if(pair instanceof ScmPair){
				Object car=((ScmPair)pair).getCar();
				if(car instanceof DatumLabelSrc){
					datumRef.put(((DatumLabelSrc)car).getLabel(),datum);
				}else if(car instanceof Token&&car.toString().equals("#;")){
					return false;
				}else{
					datum=pair;
				}
			}
		}
		return true;
	}
	public ArrayList<Object> getRemainingDatums(){
		ArrayList<Object> datums=new ArrayList<Object>();
		Object datum=nextDatum();
		while(datum!=null){
			datums.add(datum);
			datum=nextDatum();
		}
		return datums;
	}
	/*public static void main(String[] args) throws Exception{
		BufferedReader in=new BufferedReader(new InputStreamReader(System.in));
		String s=null;
		while((s=in.readLine())!=null){
			System.out.println(new Parser(s).getDatums());
		}
	}*/
}
interface Level{
	boolean add(Object obj);
	void addDot();
	Object getContent();
}
class ListLevel implements Level{
	ScmPairOrNil first;
	Object end;
	boolean last,pop;
	public ListLevel(Object car){
		pop=true;
		end=first=new ScmPair(car,ScmNil.NIL);
	}
	public ListLevel(){
		pop=last=false;
		end=first=ScmNil.NIL;
	}
	public boolean add(Object obj){
		if(last){
			((ScmPair)end).setCdr(obj);
		}else{
			ScmPair pair=new ScmPair(obj,ScmNil.NIL);
			if(first==ScmNil.NIL){
				end=first=pair;
			}else{
				((ScmPair)end).setCdr(pair);
				end=pair;
			}
		}
		return pop;
	}
	public void addDot(){
		last=true;
	}
	public Object getContent(){
		return first;
	}
}
class VectorLevel implements Level{
	ArrayList<Object> vector=new ArrayList<Object>();
	public VectorLevel(){

	}
	public boolean add(Object obj){
		vector.add(obj);
		return false;
	}
	public void addDot(){
		throw new RuntimeException("Dot in vector");
	}
	public Object getContent(){
		return vector;
	}
}
class ByteVectorLevel implements Level{
	ArrayList<Byte> vector=new ArrayList<Byte>();
	public ByteVectorLevel(){

	}
	public boolean add(Object obj){
		vector.add(((java.math.BigInteger)obj).byteValue());
		return false;
	}
	public void addDot(){
		throw new RuntimeException("Dot in bytevector");
	}
	public Object getContent(){
		return vector;
	}
}
