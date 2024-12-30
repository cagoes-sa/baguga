package parser.ast.statements

import parser.ast.{Expression, Statement}
import token.Token

case class ReturnStatement(token: Token, returnValue: Expression)
    extends Statement {
  override def statementNode(): Unit = {}
  override def tokenLiteral: String = token.literal
}
