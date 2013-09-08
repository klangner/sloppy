package com.klangner.sloppy


/**
 *   Language grammar:
 *   Expression 	:= Term ['+'|'-' Term]
 *   Term 			:= Factor ['*'|'/' Factor]
 *   Factor			:= Number | '(' Expression ')' 
 */  
object Parser{

    /** Parse can find any matching node */
    val ErrorNodeValue = (ErrorNode(""), "")
    
    /** Scanner functions */
    val scanner = Scanner
    
  	def parse(text: String): AST = {
  	    parseExpression(text)
	}

  	def parseExpression(text: String): AST = {
  	    val term1 = parseTerm(text)
  	    if(term1._1.isInstanceOf[ErrorNode]) return term1._1
  	    val operatorToken = scanner.nextToken(term1._2)
  	    if(operatorToken._1 == Scanner.OperatorToken("+")){
	  	    val term2 = parseTerm(operatorToken._2)
	  	    if(term2._1.isInstanceOf[ErrorNode]) return term2._1
	  	    return ExpressionNode(term1._1, operatorToken._1.value, term2._1)
  	    }
  	    term1._1
	}

  	def parseTerm(text: String): (AST, String) = {
  	    val token = scanner.nextToken(text)
  	    token._1 match{
  	        case Scanner.NumberToken(value) => (NumberNode(value.toDouble), token._2)
  	        case Scanner.OperatorToken(value) => 
  	            val token2 = parseTerm(token._2) 
  	            token2._1 match{
	  	        	case NumberNode(value) => (NumberNode(-value), token2._2)
	  	        	case _ => ErrorNodeValue
  	            }
  	        case _ => ErrorNodeValue
  	    }
	}
}