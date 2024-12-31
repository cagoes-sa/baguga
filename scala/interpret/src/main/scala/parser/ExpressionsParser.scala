package parser

import parser.ast.Expression
import token.TokenType

trait infixParseFn {
  def apply(e: Expression): Expression
}

trait prefixParseFn {
  def apply: Expression
}

trait ExpressionsParser {
  var prefixParseFns: Seq[prefixParseFn] = Seq.empty[prefixParseFn]
  var infixParseFns: Seq[infixParseFn] = Seq.empty[infixParseFn]

  def registerPrefix(tokenType: TokenType, fn: prefixParseFn): Unit = {
    prefixParseFns +:= fn
  }

  def registerInfix(tokenType: TokenType, fn: infixParseFn): Unit = {
    infixParseFns +:= fn
  }

}
