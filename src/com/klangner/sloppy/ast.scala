package com.klangner.sloppy

abstract class AbstractSyntaxTree
case class SumNode(left: AbstractSyntaxTree, right: AbstractSyntaxTree) extends AbstractSyntaxTree
case class IntegerNode(value: Int) extends AbstractSyntaxTree
case class FloatNode(value: Float) extends AbstractSyntaxTree
  

