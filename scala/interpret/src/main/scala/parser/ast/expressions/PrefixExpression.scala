package parser.ast.expressions

import parser.ast.Expression

case class PrefixExpression(operator: String, right: String)
    extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = ""

  override def string: String = ""
}
