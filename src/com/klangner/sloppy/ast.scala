package com.klangner.sloppy

abstract class AST
case class NumberNode(value: Double) extends AST
case class ExpressionNode(left: AST, operator: String, right: AST) extends AST
case class ErrorNode(reason: String) extends AST
  

