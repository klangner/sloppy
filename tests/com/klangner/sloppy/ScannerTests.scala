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
		val expected = (Scanner.IdentifierToken("command"), "")
		assertEquals(expected, token)
	}

	@Test 
	def twoIdentifiers() {
		val cmd = "count events"
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.IdentifierToken("count"), " events")
		assertEquals(expected, token)
	}

	@Test 
	def spaceIdentifier() {
		val cmd = " a23 "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.IdentifierToken("a23"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def keyword() {
		val cmd = " let "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.KeywordToken("let"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def delimiter() {
		val cmd = "( "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.DelimiterToken("("), " ")
		assertEquals(expected, token)
	}

	@Test 
	def integer() {
		val cmd = "24 "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.NumberToken("24"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def float1() {
		val cmd = "24.3 "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.NumberToken("24.3"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def float2() {
		val cmd = ".3 "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.NumberToken(".3"), " ")
		assertEquals(expected, token)
	}

	@Test 
	def minus() {
		val cmd = "- "
		val token = Scanner.nextToken(cmd)
		val expected = (Scanner.OperatorToken("-"), " ")
		assertEquals(expected, token)
	}
}