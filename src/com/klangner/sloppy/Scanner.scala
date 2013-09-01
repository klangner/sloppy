package com.klangner.sloppy

  
object Scanner{

	abstract class TokenType
	case object KeywordType extends TokenType
	case object OperatorType extends TokenType
  	case object IntegerType extends TokenType
  	case object FloatType extends TokenType
  	case object DelimiterType extends TokenType
  	case object SymbolType extends TokenType
  	case object EndOfTextType extends TokenType
  	case object ErrorType extends TokenType
  	
	type Token = (TokenType, String)

  	val endOfText = ((EndOfTextType, ""), "")
  	val keywords = ("let").split(' ').toSet
  	val delimeters = "()[]{};,."
  	val operators = "+-*/"
  	  	    
  	val SymbolLiteral = """(?s)([a-zA-Z_]\w+)(.*)""".r
  	val IntegerLiteral = """(?s)(0|[1-9][0-9]*)(.*)""".r
  	val FloatLiteral = """(?s)(\d*\.\d+)(.*)""".r
  	val SkipWhitespace = """(?s)\s+(.*)""".r
  	
  	
  	def nextToken(text: String): (Token, String) = {
		text match {
        	case "" => endOfText
        	case SkipWhitespace(rest) => nextToken(rest)
        	case SymbolLiteral(token, rest) => 
        	    if (keywords contains token) ((KeywordType, token), rest)
        	    else ((SymbolType, token), rest)
        	case FloatLiteral(token, rest) => ((FloatType, token), rest) 
        	case IntegerLiteral(token, rest) => ((IntegerType, token), rest)
        	case rest => {
        		val firstChar = rest(0)
        		if (delimeters contains firstChar) ((DelimiterType, firstChar.toString), rest.substring(1))
        		else if (operators contains firstChar) ((OperatorType, firstChar.toString), rest.substring(1))
        		else ((ErrorType, rest), rest.substring(1))
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