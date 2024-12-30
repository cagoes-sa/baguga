package parser.ast.statements

import parser.ast.expressions.Identifier
import parser.ast.{Expression, Statement}
import token.Token

case class LetStatement(token: Token, name: Identifier, value: Expression)
    extends Statement {
  override def statementNode(): Unit = {}
  override def tokenLiteral: String = token.literal
}
