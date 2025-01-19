package parser

import parser.Parser.EOFToken
import parser.ast.Expression
import parser.ast.expressions.{ExpressionOrdering, Identifier}
import token.TokenType
import token.TokenType.{EOF, IDENT, SEMICOLON}

trait ParserExpressions { parser: Parser =>
  final type PrefixParserFn = () => Option[Expression]
  final val prefixParserFns: Map[TokenType, PrefixParserFn] = Map(
    TokenType.IDENT -> parseIdentifier
  )

  def parseExpression(precedence: ExpressionOrdering): Option[Expression] = {
    val leftExp: Option[Expression] = prefixParserFns.get(cToken.getOrElse(EOFToken).tokenType) match {
      case Some(function: PrefixParserFn) => function()
      case None => None
    }

    leftExp

  }

  def parseExpressionMock(): Some[Expression] = {
    while ( cToken.getOrElse(EOFToken).tokenType match {
      case EOF => false
      case SEMICOLON => false
      case _ => true
    }) {
      nextTokens()
    }
    Some(Identifier(EOFToken, ""))
  }

  def parseIdentifier(): Option[Identifier] = {
    cToken match {
      case Some(token) => Some(Identifier(token, token.literal))
      case _ => None
    }
  }

  def parseIdentifierPeak(): Option[Identifier] = {
    if (expectPeek(IDENT)) {
      cToken match {
        case Some(token) => Some(Identifier(token, token.literal))
        case _ => None
      }
    } else {
      None
    }
  }

}
