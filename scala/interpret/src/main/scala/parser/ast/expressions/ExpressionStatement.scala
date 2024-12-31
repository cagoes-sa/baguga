package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class ExpressionStatement(
    token: Token,
    expression: Option[Expression]
) extends Expression {
  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = expression match {
    case Some(expression) => expression.string
    case None             => ""
  }
}
