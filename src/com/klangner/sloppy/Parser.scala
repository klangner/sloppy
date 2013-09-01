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
  	        case Scanner.IntegerType => IntegerNode(value.toInt)
  	        case Scanner.FloatType => FloatNode(value.toFloat)
//  	        case Scanner.OperatorType => 
//  	          if(value == "-") -parseValue(token._2)
//  	          else null
  	        case _ => null
  	    }
	}

	
	/**
	 * For debugging purposes
	 */
	def main(args : Array[String]): Unit = {
	    val parser = Parser
	    val ast = parser.parse("count objects")
	    println(ast)
	}
}