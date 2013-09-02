package com.klangner.sloppy

abstract class AbstractSyntaxTree
case class SumNode(left: AbstractSyntaxTree, right: AbstractSyntaxTree) extends AbstractSyntaxTree
case class NumberNode(value: Double) extends AbstractSyntaxTree
  

