package parser.ast

trait Node {
  def tokenLiteral: String
}

trait Statement extends Node {
  def statementNode(): Unit
}

trait Expression extends Node {
  def expressionNode(): Unit
}
