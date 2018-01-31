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
	private static final HashMap<String,Integer> NAME2CHAR=new HashMap<>();
	private static final HashMap<Integer,Integer> MEM2CHAR=new HashMap<>();
	private static final HashSet<Integer> DELIMITER=new HashSet<>();
	private static final HashSet<Integer> ID_INITIAL=new HashSet<>(),ID_SUBSEQUENCE=new HashSet<>();
	private static final HashSet<Integer> ID_INITIAL_TYPE=new HashSet<>(),ID_SUBSEQUENCE_TYPE=new HashSet<>();
	private static final SimpleToken DOT=SimpleToken.getToken(".");
	private static final SimpleToken COMMA=SimpleToken.getToken(",");
	private static final SimpleToken COMMA_AT=SimpleToken.getToken(",@");
	private static final SimpleToken LEFT_VECTOR=SimpleToken.getToken("#(");
	private static final SimpleToken LEFT_PAREN=SimpleToken.getToken("(");
	private static final SimpleToken RIGHT_PAREN=SimpleToken.getToken(")");
	private static final SimpleToken QUOTE=SimpleToken.getToken("\'");
	private static final SimpleToken QUASIQUOTE=SimpleToken.getToken("`");
	private static final SimpleToken BYTE8=SimpleToken.getToken("#u8(");
	private static final SimpleToken COMMENT_NEXT=SimpleToken.getToken("#;");
	private final PushbackReader in;
	private boolean foldingCase;
	static{
		NAME2CHAR.put("null",(int)'\0');
		NAME2CHAR.put("alarm",ALARM);
		NAME2CHAR.put("backspace",BACKSPACE);
		NAME2CHAR.put("tab",TABULATION);
		NAME2CHAR.put("linefeed",LINEFEED);
		NAME2CHAR.put("newline",NEWLINE);
		NAME2CHAR.put("vtab",LINE_TABULATION);
		NAME2CHAR.put("page",FORM_FEED);
		NAME2CHAR.put("return",CARRIAGE_RETURN);
		NAME2CHAR.put("escape",ESCAPE);
		NAME2CHAR.put("space",SPACE);
		NAME2CHAR.put("delete",DELETE);
		MEM2CHAR.put((int)'a',ALARM);
		MEM2CHAR.put((int)'b',BACKSPACE);
		MEM2CHAR.put((int)'t',TABULATION);
		MEM2CHAR.put((int)'n',LINEFEED);
		MEM2CHAR.put((int)'v',LINE_TABULATION);
		MEM2CHAR.put((int)'f',FORM_FEED);
		MEM2CHAR.put((int)'r',CARRIAGE_RETURN);
		MEM2CHAR.put((int)'\"',(int)'\"');
		MEM2CHAR.put((int)'\\',(int)'\\');
		MEM2CHAR.put((int)'|',(int)'|');
		DELIMITER.add((int)'(');
		DELIMITER.add((int)')');
		//delimiter.add((int)'[');//for R6RS
		//delimiter.add((int)']');//for R6RS
		DELIMITER.add((int)'\"');
		DELIMITER.add((int)'|');
		DELIMITER.add((int)';');
		ID_INITIAL.add((int)'!');
		ID_INITIAL.add((int)'$');
		ID_INITIAL.add((int)'%');
		ID_INITIAL.add((int)'&');
		ID_INITIAL.add((int)'*');
		ID_INITIAL.add((int)'/');
		ID_INITIAL.add((int)':');
		ID_INITIAL.add((int)'<');
		ID_INITIAL.add((int)'=');
		ID_INITIAL.add((int)'>');
		ID_INITIAL.add((int)'?');
		ID_INITIAL.add((int)'^');
		ID_INITIAL.add((int)'_');
		ID_INITIAL.add((int)'~');
		ID_INITIAL.add((int)'\u200C');
		ID_INITIAL.add((int)'\u200D');
		ID_INITIAL_TYPE.add((int)Character.UPPERCASE_LETTER);
		ID_INITIAL_TYPE.add((int)Character.LOWERCASE_LETTER);
		ID_INITIAL_TYPE.add((int)Character.TITLECASE_LETTER);
		ID_INITIAL_TYPE.add((int)Character.MODIFIER_LETTER);
		ID_INITIAL_TYPE.add((int)Character.OTHER_LETTER);
		ID_INITIAL_TYPE.add((int)Character.NON_SPACING_MARK);
		ID_INITIAL_TYPE.add((int)Character.LETTER_NUMBER);
		ID_INITIAL_TYPE.add((int)Character.OTHER_NUMBER);
		ID_INITIAL_TYPE.add((int)Character.DASH_PUNCTUATION);
		ID_INITIAL_TYPE.add((int)Character.CONNECTOR_PUNCTUATION);
		ID_INITIAL_TYPE.add((int)Character.OTHER_PUNCTUATION);
		ID_INITIAL_TYPE.add((int)Character.CURRENCY_SYMBOL);
		ID_INITIAL_TYPE.add((int)Character.MATH_SYMBOL);
		ID_INITIAL_TYPE.add((int)Character.MODIFIER_SYMBOL);
		ID_INITIAL_TYPE.add((int)Character.OTHER_SYMBOL);
		ID_INITIAL_TYPE.add((int)Character.PRIVATE_USE);
		ID_SUBSEQUENCE.addAll(ID_INITIAL);
		ID_SUBSEQUENCE.add((int)'+');
		ID_SUBSEQUENCE.add((int)'-');
		ID_SUBSEQUENCE.add((int)'.');
		ID_SUBSEQUENCE.add((int)'@');
		ID_SUBSEQUENCE_TYPE.addAll(ID_INITIAL_TYPE);
		ID_SUBSEQUENCE_TYPE.add((int)Character.DECIMAL_DIGIT_NUMBER);
		ID_SUBSEQUENCE_TYPE.add((int)Character.COMBINING_SPACING_MARK);
		ID_SUBSEQUENCE_TYPE.add((int)Character.ENCLOSING_MARK);
	}
	/**
	 * Construct a lexical analyzer to analysis a piece of code from a Reader
	 * @param in the source
	 */
	public Lex(Reader in){
		this(new PushbackReader(in,2),false);
	}
	/**
	 * Construct a lexical analyzer to analysis a piece of code from a Reader
	 * @param in the source
	 * @param foldingCase default to case folding or not
	 */
	public Lex(Reader in,boolean foldingCase){
		this(new PushbackReader(in,2),foldingCase);
	}
	/**
	 * Construct a lexical analyzer to analysis a piece of code from a Reader
	 * @param in the source
	 */
	public Lex(PushbackReader in){
		this(in,false);
	}
	/**
	 * Construct a lexical analyzer to analysis a piece of code from a Reader
	 * @param in the source
	 * @param foldingCase default to case folding or not
	 */
	public Lex(PushbackReader in,boolean foldingCase){
		this.in=in;
		this.foldingCase=foldingCase;
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
	public ArrayList<Token> getRemainingTokens()throws IOException{
		ArrayList<Token> tokens=new ArrayList<>();
		Token next=nextToken();
		while(next!=null){
			tokens.add(next);
			next=nextToken();
		}
		return tokens;
	}
	private void unreadIfNotEOF(int c) throws IOException{
		unreadIfNotEOF(c,in);
	}
	private static void unreadIfNotEOF(int c,PushbackReader in) throws IOException{
		if(c!=EOF)
			in.unread(c);
	}
	/**
	 * Get the next token
	 * @return the token or null if the end of the code is reached
	 */
	public Token nextToken(){
		try{
  			while(true){
				int c=in.read();
				if(isWhiteSpace(c))
					continue;
				switch(c){
					case EOF:return null;
					case '(':return LEFT_PAREN;
					case ')':return RIGHT_PAREN;
					case '`':return QUASIQUOTE;
					case '\'':return QUOTE;
					case ',':
						c=in.read();
						if(c=='@')
							return COMMA_AT;
						else{
							unreadIfNotEOF(c);
							return COMMA;
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
								return COMMENT_NEXT;
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
								return LEFT_VECTOR;
							case 'u':case 'U':
								if(in.read()=='8'&&in.read()=='(')
									return BYTE8;
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
							case EOF:
								throw new LexicalException();
							default:
								in.unread(c);
								if(Character.isLetter(c)){
									in.unread('#');
									return parseNumber(untilNextDelimiter());
								}else
									return nextLabel();
						}
						break;
					case '\"':
						return nextString();
					case '|':
						return nextVerbatimIdentifer();
					default:
						in.unread(c);
						return nextIdentiferOrNumber();
				}
			}
		}catch(IOException ex){
			return null;
		}
	}
	private void expectIgnoreCase(String str) throws IOException{
		expectIgnoreCase(str,in);
	}
	private static void expectIgnoreCase(String str,Reader in) throws IOException{
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
		return DELIMITER.contains(c)||isWhiteSpace(c);
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
	private ScmCharacter nextCharacter()throws IOException{
		String cname=untilNextDelimiter();
		int codepoint;
		if(cname.length()==1)
			codepoint=cname.codePointAt(0);
		else if(cname.length()==0){
			codepoint=in.read();
			if(codepoint==EOF)
				throw new LexicalException();
		}else if(cname.charAt(0)=='x'||cname.charAt(0)=='X'){
			codepoint=Character.toChars(Integer.valueOf(cname.substring(1),16))[0];
		}else{
			if(foldingCase)
				cname=ScmString.toFoldingCase(cname);
			if(NAME2CHAR.containsKey(cname))
				codepoint=NAME2CHAR.get(cname);
			else
				throw new LexicalException();
		}
		return ScmCharacter.getScmCharacter(codepoint);
	}
	private ScmSymbol createIdentifier(String name){
		return new ScmSymbol(foldingCase?ScmString.toFoldingCase(name):name);
	}
	private String untilNextDelimiter()throws IOException{
		StringBuilder buf=new StringBuilder();
		buf.appendCodePoint(in.read());
		int c;
		while((c=in.read())!=EOF&&!isDelimiter(c)){
			buf.appendCodePoint(c);
		}
		unreadIfNotEOF(c);
		return buf.toString();
	}
	private DatumLabel nextLabel()throws IOException{
		StringBuilder buf=new StringBuilder();
		while(true){
			int c=in.read();
			if(c=='='){
				DatumRecord.updateId(Integer.parseInt(buf.toString()));
				return new DatumLabelSrc(buf.toString());
			}else if(c=='#')
				return new DatumLabelRef(buf.toString());
			else if(Character.isDigit(c))
				buf.appendCodePoint(c);
			else
				throw new LexicalException();
		}
	}
	private ScmString nextString() throws IOException{
		StringBuilder str=new StringBuilder();
		int c=in.read();
		while(true){
			if(c=='\\'){
				c=in.read();
				if(MEM2CHAR.containsKey(Character.toLowerCase(c))){
					str.appendCodePoint(MEM2CHAR.get(c));
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
		return new ScmString(str.toString());
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
	private ScmSymbol nextVerbatimIdentifer()throws IOException{
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
		return createIdentifier(buf.toString());
	}
	private int nextHexOrMem()throws IOException{
		int c=in.read();
		if(MEM2CHAR.containsKey(Character.toLowerCase(c)))
			return MEM2CHAR.get(c);
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
	private boolean isAllSubsequent(String token){
		return token.codePoints().allMatch((c)->isSubsequent(c));
	}
	private Token nextIdentiferOrNumber()throws IOException{
		String token=untilNextDelimiter();
		if(isInitial(token.codePointAt(0))&&isAllSubsequent(token))
			return createIdentifier(token);
		int prefix=token.codePointAt(0);
		if(prefix=='.'){
			if(token.length()==1)
				return DOT;
			int snd=token.codePointAt(1);
			if((snd=='.'||snd=='+'||snd=='-'||snd=='@'||isInitial(snd))&&isAllSubsequent(token)){
				return createIdentifier(token);
			}
		}else if(prefix=='+'||prefix=='-'){
			if(token.length()==1){
				return createIdentifier(token);
			}
			int c=token.codePointAt(1);
			if(c=='.'&&token.length()>=3){
				c=token.codePointAt(2);
				if((c=='.'||c=='+'||c=='-'||c=='@'||isInitial(c))&&isAllSubsequent(token)){
					return createIdentifier(token);
				}
			}
			if((c=='+'||c=='-'||c=='@'||isInitial(c))&&isAllSubsequent(token)){
				try{
					return parseNumber(token);
				}catch(RuntimeException ex){
					return createIdentifier(token);
				}
			}
		}
		return parseNumber(token);
	}
	private static boolean isInitial(int c){
		return (c>='a'&&c<='z')||(c>='A'&&c<='Z')||ID_INITIAL.contains(c)||(c>127&&ID_INITIAL_TYPE.contains(Character.getType(c)));
	}
	private static boolean isSubsequent(int c){
		return (c>='a'&&c<='z')||(c>='A'&&c<='Z')||(c>='0'&&c<='9')||ID_SUBSEQUENCE.contains(c)||(c>127&&ID_SUBSEQUENCE_TYPE.contains(Character.getType(c)));
	}
	private static ScmNumber parseNumber(String literal)throws IOException{
		PushbackReader in=new PushbackReader(new StringReader(literal));
		int base=10;
		byte exact=0;//1 mean exact, -1 means inexact,0 means not mentioned
		int prefix=in.read();
		while(prefix=='#'){
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
		}
		unreadIfNotEOF(prefix,in);
		ScmReal real=parseReal(in,base,exact);
		prefix=in.read();
		ScmNumber result=null;
		switch(prefix){
			case 'i':
				result=new ScmComplexRectangular(ScmInteger.ZERO,real);
				break;
			case '@':
				result=new ScmComplexPolar(real,parseReal(in,base,exact));
				break;
			case '+':case '-':
				in.unread(prefix);
				result=new ScmComplexRectangular(real,parseReal(in,base,exact));
				if(in.read()!='i')
					throw new LexicalException();
				break;
			case EOF:result=real;break;
		}
		if(in.read()!=EOF)
			throw new LexicalException();
		else
			return result;
	}
	private static ScmReal parseReal(PushbackReader in,int base,int exact) throws IOException{
		boolean neg=false;
		int prefix=in.read();
		if(prefix=='+'){
			prefix=in.read();
		}else if(prefix=='-'){
			neg=true;
			prefix=in.read();
		}
		BigInteger num=BigInteger.ZERO,b=BigInteger.valueOf(base);
		DigitVerifier digitCheck=DigitVerifier.getDigitVerifier(base);
		boolean ate=false;
		while(digitCheck.verify(prefix)){
			num=num.multiply(b).add(BigInteger.valueOf(Character.digit(prefix,base)));
			prefix=in.read();
			ate=true;
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
				ate=true;
			}
			if(exact==0)
				exact=-1;
		}else if(ate&&prefix=='/'){
			BigInteger dorm=BigInteger.ZERO;
			prefix=in.read();
			while(digitCheck.verify(prefix)){
				dorm=dorm.multiply(b).add(BigInteger.valueOf(Character.digit(prefix,base)));
				prefix=in.read();
			}
			unreadIfNotEOF(prefix,in);
			return new ScmRational(new ScmInteger(neg?num.negate():num),new ScmInteger(dorm));
		}else if(prefix=='i'||prefix=='I'){
			if(ate){
				in.unread('i');
				return new ScmInteger(neg?num.negate():num);
			}else{
				prefix=in.read();
				if(prefix=='n'||prefix=='N'){
					expectIgnoreCase("f.0",in);
					return neg?ScmSpecialReal.NEGATIVE_INF:ScmSpecialReal.POSITIVE_INF;
				}else{
					unreadIfNotEOF(prefix,in);
					in.unread('i');
					return neg?ScmInteger.ONE.negate():ScmInteger.ONE;
				}
			}
		}else if((!ate)&&(prefix=='n'||prefix=='N')){
			expectIgnoreCase("an.0",in);
			return ScmSpecialReal.POSITIVE_NAN;
		}else
			real=new BigDecimal(num);
		if(!ate)
			throw new LexicalException();
		if(prefix=='e'||prefix=='E'){
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
			if(exact==0)
				exact=-1;
		}
		unreadIfNotEOF(prefix,in);
		real=neg?real.negate():real;
		if(exact==-1)
			return new ScmFloatingPointNumber(real);
		else{
			try{
				num=real.toBigIntegerExact();
				return new ScmInteger(num);
			}catch(RuntimeException e){
				return new ScmFloatingPointNumber(real).toScmRational();
			}
		}
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
	static final DigitVerifier[] LOADED=new DigitVerifier[37];
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
		if(LOADED[base]==null)
			LOADED[base]=new DigitVerifier(base);
		return LOADED[base];
	}
	public boolean verify(int c){
		return c>=0&&c<128&&digits.get(c);
	}
}