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
		assertEquals(IntegerNode(2), ast)
	}
 
	@Test 
	def valueFloat() {
		val ast = Parser.parseExpression(".2")
		assertEquals(FloatNode(0.2f), ast)
	}
 
	@Test 
	def valueMinusInt() {
		val ast = Parser.parseExpression("-2")
//		assertEquals(IntegerNode(-2), ast)
	}
}