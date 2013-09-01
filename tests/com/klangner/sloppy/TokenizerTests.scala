package com.klangner.sloppy

import org.scalatest.FunSuite

class TokenizerTests extends FunSuite {
 
	test("Empty command") {
		val cmd = "text2.hide()"
		val tokenizer = Tokenizer
		val token = tokenizer.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[tokenizer.SymbolType.type])
		assert(token._2 == "text2")
	}
}