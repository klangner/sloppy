package com.klangner.sloppy

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ScannerTests extends AssertionsForJUnit {
 
	@Test 
	def symbol() {
		val cmd = "command"
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.SymbolType.type])
		assertEquals("", token._2)
	}

	@Test 
	def twoSymbols() {
		val cmd = "symbol1 symbol2"
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.SymbolType.type])
		assertEquals(" symbol2", token._2)
	}

	@Test 
	def spaceSymbol() {
		val cmd = " symbol "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.SymbolType.type])
		assertEquals("symbol", token._1._2)
		assertEquals(" ", token._2)
	}

	@Test 
	def keyword() {
		val cmd = " let "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.KeywordType.type])
		assertEquals("let", token._1._2)
		assertEquals(" ", token._2)
	}

	@Test 
	def delimeter() {
		val cmd = "( "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.DelimiterType.type])
		assertEquals("(", token._1._2)
		assertEquals(" ", token._2)
	}

	@Test 
	def integer() {
		val cmd = "24 "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.IntegerType.type])
		assertEquals("24", token._1._2)
		assertEquals(" ", token._2)
	}

	@Test 
	def float1() {
		val cmd = "24.3 "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.FloatType.type])
		assertEquals("24.3", token._1._2)
		assertEquals(" ", token._2)
	}

	@Test 
	def float2() {
		val cmd = ".3 "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		assert(tokenType.isInstanceOf[Scanner.FloatType.type])
		assertEquals(".3", token._1._2)
		assertEquals(" ", token._2)
	}
}