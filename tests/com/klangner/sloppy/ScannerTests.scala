package com.klangner.sloppy

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ScannerTests extends AssertionsForJUnit {
 
	@Test 
	def identifier() {
		val cmd = "command"
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.IdentifierType, "command"), "")
		assertEquals(expected, token)
	}

	@Test 
	def twoIdentifiers() {
		val cmd = "count events"
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.IdentifierType, "count"), " events")
		assertEquals(expected, token)
	}

	@Test 
	def spaceIdentifier() {
		val cmd = " a23 "
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.IdentifierType, "a23"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def keyword() {
		val cmd = " let "
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.KeywordType, "let"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def delimiter() {
		val cmd = "( "
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.DelimiterType, "("), " ")
		assertEquals(expected, token)
	}

	@Test 
	def integer() {
		val cmd = "24 "
		val token = Scanner.nextToken(cmd)
		val tokenType = token._1._1 
		val expected = ((Scanner.IntegerType, "24"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def float1() {
		val cmd = "24.3 "
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.FloatType, "24.3"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def float2() {
		val cmd = ".3 "
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.FloatType, ".3"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def minus() {
		val cmd = "- "
		val token = Scanner.nextToken(cmd)
		val expected = ((Scanner.OperatorType, "-"), " ")
		assertEquals(expected, token)
	}
}