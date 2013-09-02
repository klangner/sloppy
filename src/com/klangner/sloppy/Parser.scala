package com.klangner.sloppy


  
object Parser{

    val scanner = Scanner
    
  	def parse(text: String): AbstractSyntaxTree = {
  	    parseExpression(text)
	}

  	def parseExpression(text: String): AbstractSyntaxTree = {
  	    parseValue(text)
	}

  	def parseValue(text: String): AbstractSyntaxTree = {
  	    val token = scanner.nextToken(text)
  	    val value = token._1._2
  	    token._1._1 match{
  	        case Scanner.NumberType => NumberNode(value.toDouble)
  	        case Scanner.OperatorType => 
  	            parseValue(token._2) match{
	  	        	case NumberNode(value) => NumberNode(-value)
	  	        	case _ => null
  	            }
  	        case _ => null
  	    }
	}
}