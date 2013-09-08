package com.klangner.sloppy

  
object Scanner{

	abstract class TokenType(text:String){
	    val value = text
	}
	case class KeywordToken(text:String) extends TokenType(text)
	case class OperatorToken(text:String) extends TokenType(text)
  	case class NumberToken(text:String) extends TokenType(text)
  	case class DelimiterToken(text:String) extends TokenType(text)
  	case class IdentifierToken(text:String) extends TokenType(text)
  	case class EndOfTextToken extends TokenType("")
  	case class ErrorToken(text:String) extends TokenType(text)
  	
  	val endOfText = (EndOfTextToken(), "")
  	val keywords = ("let").split(' ').toSet
  	val delimeters = "()[]{};,."
  	val operators = "+-*/"
  	  	    
  	val IdentifierLiteral = """(?s)([a-zA-Z_]\w+)(.*)""".r
  	val NumberLiteral = """(?s)(\d*\.?\d+)(.*)""".r
  	val SkipWhitespace = """(?s)\s+(.*)""".r
  	
  	
  	def nextToken(text: String): (TokenType, String) = {
		text match {
        	case "" => endOfText
        	case SkipWhitespace(rest) => nextToken(rest)
        	case IdentifierLiteral(token, rest) => 
        	    if (keywords contains token) (KeywordToken(token), rest)
        	    else (IdentifierToken(token), rest)
        	case NumberLiteral(token, rest) => (NumberToken(token), rest) 
        	case rest => {
        		val firstChar = rest(0)
        		if (delimeters contains firstChar) (DelimiterToken(firstChar.toString), rest.substring(1))
        		else if (operators contains firstChar) (OperatorToken(firstChar.toString), rest.substring(1))
        		else (ErrorToken(rest), rest.substring(1))
        	}	
        }
	}

	
	/**
	 * For testing purposes
	 */
	def main(args : Array[String]): Unit = {
	    val scanner = Scanner
	    val token = scanner.nextToken("count objects")
	    println(token)
	}
}