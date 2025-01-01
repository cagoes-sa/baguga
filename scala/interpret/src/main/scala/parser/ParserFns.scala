package parser

import errors.ParserError
import parser.ast.Expression
import parser.ast.expressions.{
  ExpressionOrdering,
  Identifier,
  IntegerLiteral,
  PrefixExpression
}
import token.{Token, TokenType}

object ParserFns {
  type ParseFn =
    (Token, Option[Token]) => (Option[Expression], Seq[ParserError])
  def prefixParseFns(p: Parser): Map[TokenType, ParseFn] =
    Map(
      TokenType.IDENT -> parseIdentifierExpression,
      TokenType.INT -> parseIntegerLiteralExpression,
      TokenType.BANG -> parsePrefixExpression(p),
      TokenType.MINUS -> parsePrefixExpression(p)
    )

  private def parseIdentifierExpression(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    (Some(Identifier(c, c.literal)), Seq.empty[ParserError])
  }

  private def parseIntegerLiteralExpression(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    c.literal.toIntOption match {
      case Some(integer) =>
        (Some(IntegerLiteral(c, integer)), Seq.empty[ParserError])
      case None =>
        (None, Seq(ParserError(s"Token $c cannot be parsed into integer")))
    }
  }

  private def parsePrefixExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {

    val (optionC, nextOptionP) = p.getTokenPointers
    optionC match {
      case Some(token) =>
        p.parseExpression(ExpressionOrdering.Prefix, token, nextOptionP) match {
          case (Some(expression), errors) =>
            (Some(PrefixExpression(c, c.literal, expression)), errors)
          case (None, errors: Seq[ParserError]) =>
            (
              None,
              errors :+ ParserError(
                s"Prefix operator for token $c had the following expression failed"
              )
            )
        }
    }

  }

}
