package com.klangner.sloppy

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ParserTests extends AssertionsForJUnit {
 
	@Test 
	def valueInt() {
		val ast = Parser.parseExpression("2")
		assertEquals(NumberNode(2), ast)
	}
 
	@Test 
	def valueFloat() {
		val ast = Parser.parseExpression(".2")
		assertEquals(NumberNode(0.2), ast)
	}
 
	@Test 
	def valueMinusInt() {
		val ast = Parser.parseExpression("-2")
		assertEquals(NumberNode(-2), ast)
	}

	@Test 
	def valueMinusMinusInt() {
		val ast = Parser.parseExpression("--2")
		assertEquals(NumberNode(2), ast)
	}

	@Test 
	def valueMinusFloat() {
		val ast = Parser.parseExpression("-2.4")
		assertEquals(NumberNode(-2.4), ast)
	}

	@Test 
	def sum() {
		val ast = Parser.parseExpression("3+2.4")
		val expected = ExpressionNode(NumberNode(3), "+", NumberNode(2.4))
		assertEquals(expected, ast)
	}
}