package com.klangner.sloppy


  
object Parser{

    val scanner = Scanner
    
  	def parse(text: String): AbstractSyntaxTree = {
  	    parseExpression(text)
	}

  	def parseExpression(text: String): AbstractSyntaxTree = {
  	    IntegerNode(0)
	}

  	def parseValue(text: String): AbstractSyntaxTree = {
  	    val token = scanner.nextToken(text)
  	    token._1._1 match{
  	        case Scanner.IntegerType => 1
  	        case Scanner.FloatType => 0.1
  	    }
  	    
  	    null
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