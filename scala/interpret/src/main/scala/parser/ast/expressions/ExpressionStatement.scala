package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class ExpressionStatement(
    token: Token
) extends Expression {
  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal
}
