package com.klangner.sloppy

  
object Tokenizer{

	abstract class TokenType
	case object KeywordToken extends TokenType
  	case object IntegerType extends TokenType
  	case object FloatingPointType extends TokenType
  	case object DelimiterType extends TokenType
  	case object SymbolType extends TokenType
  	case object EndOfTextType extends TokenType
  	case object ErrorType extends TokenType
  	
	type Token = (TokenType, String)

  	val endOfText = ((EndOfTextType, ""), "")
  	val keywords = ("val =").split(' ').toSet
  	val SymbolLiteral = "(?s)'%s(.*)".r
  	val SkipWhitespace = """(?s)\s+(.*)""".r
  	
  	
  	def nextToken(text: String): (Token, String) = {
		text match {
        	case "" => endOfText
        	case SymbolLiteral(token, _, _, rest) => ((SymbolType, token), rest)
        	case SkipWhitespace(rest) => nextToken(rest)
        	case rest => {
        		val firstChar = rest(0)
        		if ("()[]{};,." contains firstChar) ((DelimiterType, firstChar.toString), rest.substring(1))
        		else ((ErrorType, rest), rest.substring(1))
        	}	
        }
	}

	
	/**
	 * For testing purposes
	 */
	def main(args : Array[String]) = {
	    val tokenizer = Tokenizer
	    val token = tokenizer.nextToken("count objects")
	    println(token)
	}
}