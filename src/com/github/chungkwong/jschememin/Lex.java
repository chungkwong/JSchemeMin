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
import java.io.*;
import java.math.*;
import java.util.*;
/**
 * A hand written lexical analyzer for Scheme(the intended target is R7RS small language)
 */
public final class Lex{
	private static final int EOF=-1;
	private static final int ALARM='\u0007',BACKSPACE='\u0008',TABULATION='\u0009',LINEFEED='\n',NEWLINE='\n'
		,LINE_TABULATION='\u000B',FORM_FEED='\u000C',CARRIAGE_RETURN='\r',ESCAPE='\u001B',SPACE='\u0020'
		,DELETE='\u007F',NEXT_LINE='\u0085',LINE_SEPARATOR='\u2028',PARAGRAPH_SEPARATOR='\u2029';
	private static final HashMap<String,Integer> name2char=new HashMap<>();
	private static final HashMap<Integer,Integer> mem2char=new HashMap<>();
	private static final HashSet<Integer> delimiter=new HashSet<>();
	private static final HashSet<Integer> idInitial=new HashSet<>(),idSubsequent=new HashSet<>();
	private static final HashSet<Integer> idInitialType=new HashSet<>(),idSubsequentType=new HashSet<>();
	private final PushbackReader in;
	private boolean foldingCase=false;
	final HashMap<String,Object> datums=new HashMap<>();
	static{
		name2char.put("null",(int)'\0');
		name2char.put("alarm",ALARM);
		name2char.put("backspace",BACKSPACE);
		name2char.put("tab",TABULATION);
		name2char.put("linefeed",LINEFEED);
		name2char.put("newline",NEWLINE);
		name2char.put("vtab",LINE_TABULATION);
		name2char.put("page",FORM_FEED);
		name2char.put("return",CARRIAGE_RETURN);
		name2char.put("escape",ESCAPE);
		name2char.put("space",SPACE);
		name2char.put("delete",DELETE);
		mem2char.put((int)'a',ALARM);
		mem2char.put((int)'b',BACKSPACE);
		mem2char.put((int)'t',TABULATION);
		mem2char.put((int)'n',LINEFEED);
		mem2char.put((int)'v',LINE_TABULATION);
		mem2char.put((int)'f',FORM_FEED);
		mem2char.put((int)'r',CARRIAGE_RETURN);
		mem2char.put((int)'\"',(int)'\"');
		mem2char.put((int)'\\',(int)'\\');
		mem2char.put((int)'|',(int)'|');
		delimiter.add((int)'(');
		delimiter.add((int)')');
		//delimiter.add((int)'[');//for R6RS
		//delimiter.add((int)']');//for R6RS
		delimiter.add((int)'\"');
		delimiter.add((int)'|');
		delimiter.add((int)';');
		idInitial.add((int)'!');
		idInitial.add((int)'$');
		idInitial.add((int)'%');
		idInitial.add((int)'&');
		idInitial.add((int)'*');
		idInitial.add((int)'/');
		idInitial.add((int)':');
		idInitial.add((int)'<');
		idInitial.add((int)'=');
		idInitial.add((int)'>');
		idInitial.add((int)'?');
		idInitial.add((int)'^');
		idInitial.add((int)'_');
		idInitial.add((int)'~');
		idInitial.add((int)'\u200C');
		idInitial.add((int)'\u200D');
		idInitialType.add((int)Character.UPPERCASE_LETTER);
		idInitialType.add((int)Character.LOWERCASE_LETTER);
		idInitialType.add((int)Character.TITLECASE_LETTER);
		idInitialType.add((int)Character.MODIFIER_LETTER);
		idInitialType.add((int)Character.OTHER_LETTER);
		idInitialType.add((int)Character.NON_SPACING_MARK);
		idInitialType.add((int)Character.LETTER_NUMBER);
		idInitialType.add((int)Character.OTHER_NUMBER);
		idInitialType.add((int)Character.DASH_PUNCTUATION);
		idInitialType.add((int)Character.CONNECTOR_PUNCTUATION);
		idInitialType.add((int)Character.OTHER_PUNCTUATION);
		idInitialType.add((int)Character.CURRENCY_SYMBOL);
		idInitialType.add((int)Character.MATH_SYMBOL);
		idInitialType.add((int)Character.MODIFIER_SYMBOL);
		idInitialType.add((int)Character.OTHER_SYMBOL);
		idInitialType.add((int)Character.PRIVATE_USE);
		idSubsequent.addAll(idInitial);
		idSubsequent.add((int)'+');
		idSubsequent.add((int)'-');
		idSubsequent.add((int)'.');
		idSubsequent.add((int)'@');
		idSubsequentType.addAll(idInitialType);
		idSubsequentType.add((int)Character.DECIMAL_DIGIT_NUMBER);
		idSubsequentType.add((int)Character.COMBINING_SPACING_MARK);
		idSubsequentType.add((int)Character.ENCLOSING_MARK);
	}
	/**
	 * Construct a lexical analyzer to analysis a piece of code from a Reader
	 * @param in the source
	 */
	public Lex(Reader in){
		this.in=new PushbackReader(in,2);
	}
	/**
	 * Construct a lexical analyzer to analysis a piece of code as String
	 * @param code the source
	 */
	public Lex(String code){
		this(new StringReader(code));
	}
	/**
	 * Collect all remaining token
	 * @return
	 * @throws java.io.IOException
	 */
	public ArrayList<Object> getRemainingTokens()throws IOException{
		ArrayList<Object> tokens=new ArrayList<>();
		Object next=nextToken();
		while(next!=null){
			tokens.add(next);
			next=nextToken();
		}
		return tokens;
	}
	private void unreadIfNotEOF(int c) throws IOException{
		if(c!=EOF)
			in.unread(c);
	}
	/**
	 * Get the next token
	 * @return the token or null if the end of the code is reached
	 */
	public Object nextToken(){
		try{
  			while(true){
				int c=in.read();
				if(isWhiteSpace(c))
					continue;
				switch(c){
					case EOF:
						return null;
					case '(':case ')':case '`':case '\''://case '[':case ']':
						return Token.getToken(String.valueOf((char)c));
					case ',':
						c=in.read();
						if(c=='@')
							return Token.getToken(",@");
						else{
							unreadIfNotEOF(c);
							return Token.getToken(",");
						}
					case ';':
						eatLineRemaining();
						break;
					case '#':
						c=in.read();
						switch(c){
							case '|':
								eatNestedComment();
								break;
							case '!':
								c=Character.toLowerCase(in.read());
								if(c=='f'||c=='F'){
									foldingCase=true;
									expectIgnoreCase("old-case");
								}else if(c=='n'||c=='N'){
									foldingCase=false;
									expectIgnoreCase("o-fold-case");
								}else
									throw new LexicalException();
								break;
							case ';':
								return Token.getToken("#;");
							case '\\':
								return nextCharacter();
							case 't':case 'T':
								c=in.read();
								if(c=='r'||c=='R')
									expectIgnoreCase("ue");
								else
									unreadIfNotEOF(c);
								return ScmBoolean.TRUE;
							case 'f':case 'F':
								c=in.read();
								if(c=='a'||c=='A')
									expectIgnoreCase("lse");
								else
									unreadIfNotEOF(c);
								return ScmBoolean.FALSE;
							case '(':
								return Token.getToken("#(");
							case 'u':case 'U':
								if(in.read()=='8'&&in.read()=='(')
									return Token.getToken("#u8(");
								else
									throw new LexicalException();
							/*case '\'':
								return new Token("#\'");
							case '`':
								return new Token("#`");
							case ',':
								if(curr+1<len&&code.charAt(curr+1)=='@'){
									++curr;
									return new Token("#,@");
								}else
									return new Token("#,");
							case 'v':
								if(curr+3<=len&&code.substring(curr,curr+3).equals("u8(")){
									curr+=3;
									return new Token("#vu8(");
								}
								break;*/
							default:
								in.unread(c);
								if(Character.isLetter(c))
									return nextNumber('#');
								else
									return nextLabel();
						}
						break;
					case '\"':
						return nextString();
					case '|':
						return nextVerbatimIdentifer();
					default:
						return nextIdentiferOrNumber(c);
				}
			}
		}catch(IOException ex){
			return null;
		}
	}
	private void expectIgnoreCase(String str) throws IOException{
		for(int i=0;i<str.length();i++)
			if(Character.toLowerCase(in.read())!=Character.toLowerCase(str.charAt(i)))
				throw new LexicalException();
	}
	private static boolean isWhiteSpace(int c){
		switch(c){
			case TABULATION:
			case LINEFEED:
			case CARRIAGE_RETURN:
			case LINE_TABULATION:
			case FORM_FEED:
			case NEXT_LINE:
				return true;
		}
		int type=Character.getType(c);
		return type==Character.SPACE_SEPARATOR||type==Character.LINE_SEPARATOR||type==Character.PARAGRAPH_SEPARATOR;
	}
	private static boolean isDelimiter(int c){
		return delimiter.contains(c)||isWhiteSpace(c);
	}
	private void eatLineRemaining()throws IOException{
		int c=in.read();
		while(c!=EOF&&c!=LINEFEED&&c!=CARRIAGE_RETURN&&c!=NEXT_LINE&&c!=LINE_SEPARATOR&&c!=PARAGRAPH_SEPARATOR)
			c=in.read();
	}
	private void eatNestedComment()throws IOException{
		int lv=1;
		int ch;
		while((ch=in.read())!=EOF){
			if(ch=='#'){
				ch=in.read();
				if(ch=='|')
					++lv;
				else if(ch=='#')
					in.unread(ch);
			}else if(ch=='|'){
				ch=in.read();
				if(ch=='#'){
					--lv;
					if(lv==0)
						break;
				}else if(ch=='|')
					in.unread(ch);
			}
		}
	}
	private int nextCharacter()throws IOException{
		String cname=untilNextDelimiter();
		if(cname.length()==1)
			return cname.codePointAt(0);
		else if(cname.charAt(0)=='x'||cname.charAt(0)=='X'){
			return Character.toChars(Integer.valueOf(cname.substring(1),16))[0];
		}else{
			if(foldingCase)
				cname=ScmString.toFoldingCase(cname);
			return (int)name2char.get(cname);
		}
	}
	private Identifier createIdentifier(String name){
		return new Identifier(foldingCase?ScmString.toFoldingCase(name):name);
	}
	private String untilNextDelimiter()throws IOException{
		StringBuilder buf=new StringBuilder();
		buf.appendCodePoint(in.read());
		int c;
		while((c=in.read())!=EOF&&!isDelimiter(c)&&c!='#'){
			buf.appendCodePoint(c);
		}
		unreadIfNotEOF(c);
		return buf.toString();
	}
	private DatumLabel nextLabel()throws IOException{
		StringBuilder buf=new StringBuilder();
		while(true){
			int c=in.read();
			if(c=='=')
				return new DatumLabelSrc(buf.toString());
			else if(c=='#')
				return new DatumLabelRef(buf.toString());
			else if(Character.isDigit(c))
				buf.appendCodePoint(c);
			else
				throw new LexicalException();
		}
	}
	private String nextString() throws IOException{
		StringBuilder str=new StringBuilder();
		int c=in.read();
		while(true){
			if(c=='\\'){
				c=in.read();
				if(mem2char.containsKey(Character.toLowerCase(c))){
					str.appendCodePoint(mem2char.get(c));
					c=in.read();
				}else if(c=='x'){
					int point=0;
					while((c=in.read())!=';'&&c!=EOF)
						point=point*16+Character.digit(c,16);
					str.appendCodePoint(point);
					c=in.read();
				}else{
					c=eatLineEndingInString(c);
				}
			}else if(c=='\"'){
				break;
			}else if(c==EOF){
				throw new LexicalException();
			}else{
				str.appendCodePoint(c);
				c=in.read();
			}
		}
		return str.toString();
	}
	private int eatLineEndingInString(int c)throws IOException{
		while(c!=EOF&&isIntraLineSpace(c))
			c=in.read();
		switch(c){
			case LINEFEED:
			case NEXT_LINE:
			case LINE_SEPARATOR:
				c=in.read();
				break;
			case CARRIAGE_RETURN:
				c=in.read();
				if(c==LINEFEED||c==NEXT_LINE)
					c=in.read();
				break;
			default:
				throw new RuntimeException("Illegal string format");
		}
		while(c!=EOF&&isIntraLineSpace(c))
			c=in.read();
		return c;
	}
	private static boolean isIntraLineSpace(int c){
		return c==TABULATION||Character.getType(c)==Character.SPACE_SEPARATOR;
	}
	private String nextVerbatimIdentifer()throws IOException{
		int c=in.read();
		StringBuilder buf=new StringBuilder();
		while(c!='|'&&c!=EOF){
			if(c=='\\'){
				buf.appendCodePoint(nextHexOrMem());
			}else{
				buf.appendCodePoint(c);
			}
			c=in.read();
		}
		return foldingCase?ScmString.toFoldingCase(buf.toString()):buf.toString();
	}
	private int nextHexOrMem()throws IOException{
		int c=in.read();
		if(mem2char.containsKey(Character.toLowerCase(c)))
			return mem2char.get(c);
		else if(c=='x'){
			int point=0;
			while((c=in.read())!=';'){
				point=point*16+Character.digit(c,16);
			}
			return point;
		}else if(c=='|')
			return '|';
		else{
			throw new LexicalException();
		}
	}
	private Object nextIdentiferOrNumber(int prefix)throws IOException{
		if(isInitial(prefix))
			return nextIdentifer(prefix);
		if(prefix=='.'){
			int c=in.read();
			if(c==EOF||isDelimiter(c)){
				return Token.getToken(".");
			}
			in.unread(c);
			if(c=='.'||c=='+'||c=='-'||c=='@'||isInitial(c)){
				return nextIdentifer(prefix);
			}
		}else if(prefix=='+'||prefix=='-'){
			int c=in.read();
			if(c==EOF||isDelimiter(c)){
				return createIdentifier(Character.toString((char)prefix));
			}
			if(c=='+'||c=='-'||c=='@'||isInitial(c)){
				in.unread(c);
				return nextIdentifer(prefix);
			}
			if(c=='.'){
				c=in.read();
				unreadIfNotEOF(c);
				in.unread('.');
				if(c=='.'||c=='+'||c=='-'||c=='@'||isInitial(c)){
					return nextIdentifer(prefix);
				}
			}else
				in.unread(c);
		}
		return nextNumber(prefix);
	}
	private Identifier nextIdentifer(int prefix)throws IOException{
		StringBuilder str=new StringBuilder();
		str.appendCodePoint(prefix);
		prefix=in.read();
		while(prefix!=EOF&&isSubsequent(prefix)){
			str.appendCodePoint(prefix);
			prefix=in.read();
		}
		unreadIfNotEOF(prefix);
		return createIdentifier(str.toString());
	}
	private static boolean isInitial(int c){
		return (c>='a'&&c<='z')||(c>='A'&&c<='Z')||idInitial.contains(c)||(c>127&&idInitialType.contains(Character.getType(c)));
	}
	private static boolean isSubsequent(int c){
		return (c>='a'&&c<='z')||(c>='A'&&c<='Z')||(c>='0'&&c<='9')||idSubsequent.contains(c)||(c>127&&idSubsequentType.contains(Character.getType(c)));
	}
	private Object nextNumber(int prefix)throws IOException{
		int base=10;
		byte exact=0;//1 mean exact, -1 means inexact,0 means not mentioned
		if(prefix=='#'){
			do{
				switch(in.read()){
					case 'i':case 'I':exact=-1;break;
					case 'e':case 'E':exact=1;break;
					case 'b':case 'B':base=2;break;
					case 'o':case 'O':base=8;break;
					case 'd':case 'D':base=10;break;
					case 'x':case 'X':base=16;break;
					default:throw new LexicalException();
				}
				prefix=in.read();
			}while(prefix=='#');
		}
		Object real=nextReal(prefix,exact,base);
		switch(prefix){
			case 'i':
				return new Tuple(BigInteger.ZERO,real);
			case '@':
				return new Tuple(real,nextReal(in.read(),exact,base));
			case '+':
			case '-':
				return new Tuple(real,nextReal(prefix,exact,base));
			default:
				unreadIfNotEOF(prefix);
				return real;
		}
	}
	private Object nextReal(int prefix,byte exact,int base)throws IOException{
		boolean neg=false;
		if(prefix=='+'){
			prefix=in.read();
		}else if(prefix=='-'){
			neg=true;
			prefix=in.read();
		}
		if(prefix=='i'){
			in.unread('i');
			return neg?BigInteger.ONE.negate():BigInteger.ONE;
		}
		BigInteger num=BigInteger.ZERO,b=BigInteger.valueOf(base);
		DigitVerifier digitCheck=DigitVerifier.getDigitVerifier(base);
		while(digitCheck.verify(prefix)){
			num=num.multiply(b).add(BigInteger.valueOf(Character.digit(prefix,base)));
			prefix=in.read();
		}
		BigDecimal real;
		if(prefix=='.'){
			BigDecimal ratio=BigDecimal.ONE.divide(BigDecimal.valueOf(base)),pos=ratio;
			real=new BigDecimal(num);
			prefix=in.read();
			while(digitCheck.verify(prefix)){
				real=BigDecimal.valueOf(Character.digit(prefix,base)).multiply(pos).add(real);
				pos=pos.multiply(ratio);
				prefix=in.read();
			}
		}else if(prefix=='/'){
			BigInteger dorm=BigInteger.ZERO;
			prefix=in.read();
			while(digitCheck.verify(prefix)){
				dorm=dorm.multiply(b).add(BigInteger.valueOf(Character.digit(prefix,base)));
				prefix=in.read();
			}
			unreadIfNotEOF(prefix);
			return new Rational(neg?num.negate():num,dorm);
		}else if(prefix=='i'||prefix=='I'){
			prefix=in.read();
			if(prefix=='n'||prefix=='N'){
				expectIgnoreCase("f.0");
				return neg?Double.NEGATIVE_INFINITY:Double.POSITIVE_INFINITY;
			}else{
				in.unread(prefix);
				in.unread('i');
				return neg?num.negate():num;
			}
		}else if(prefix=='n'){
			expectIgnoreCase("an.0");
			return neg?-Double.NaN:Double.NaN;
		}else
			real=new BigDecimal(num);
		if(prefix=='e'){
			prefix=in.read();
			boolean negexp=false;
			if(prefix=='+'){
				prefix=in.read();
			}else if(prefix=='-'){
				negexp=true;
				prefix=in.read();
			}
			int exp=0;
			while(Character.isDigit(prefix)){
				exp=exp*10+Character.digit(prefix,10);
				prefix=in.read();
			}
			real=real.movePointRight(negexp?-exp:exp);
		}
		return neg?real.negate():real;
	}
	public static void main(String[] args)throws IOException{
		BufferedReader in=new BufferedReader(new InputStreamReader(System.in));
		String s;
		while((s=in.readLine())!=null){
			System.out.println(new Lex(s).getRemainingTokens());
		}
	}
}
class DigitVerifier{
	BitSet digits=new BitSet(128);
	static final DigitVerifier[] loaded=new DigitVerifier[37];
	private DigitVerifier(int base){
		if(base<2||base>36)
			throw new RuntimeException();
		digits.set('0','0'+Math.min(10,base));
		if(base>10){
			digits.set('a','a'+(base-10));
			digits.set('A','A'+(base-10));
		}
	}
	public static DigitVerifier getDigitVerifier(int base){
		if(loaded[base]==null)
			loaded[base]=new DigitVerifier(base);
		return loaded[base];
	}
	public boolean verify(int c){
		return c>=0&&c<128&&digits.get(c);
	}
}