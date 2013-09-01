package com.klangner.sloppy

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ParserTests extends AssertionsForJUnit {
 
	@Test 
	def symbol() {
		val ast = Parser.parse("2")
		assertEquals(IntegerNode(2), ast)
	}
}