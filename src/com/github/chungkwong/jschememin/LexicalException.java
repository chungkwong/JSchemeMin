/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.github.chungkwong.jschememin;

/**
 *
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public class LexicalException extends RuntimeException {

	/**
	 * Creates a new instance of <code>LexicalException</code> without detail message.
	 */
	public LexicalException() {
	}


	/**
	 * Constructs an instance of <code>LexicalException</code> with the specified detail message.
	 * @param msg the detail message.
	 */
	public LexicalException(String msg) {
		super(msg);
	}
}
