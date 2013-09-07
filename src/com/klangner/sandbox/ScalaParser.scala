package com.klangner.sandbox

import scala.util.parsing.combinator.JavaTokenParsers

class ScalaParser extends JavaTokenParsers {   
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends ScalaParser {
    def main(args: Array[String]) {
        val input = "3*(2+5)"
    	println("input : " + input)
    	println(parseAll(expr, input))
    }
}