package parser

import parser.Parser.EOFToken
import parser.ast.Expression
import parser.ast.expressions.{BooleanLiteral, ExpressionOrdering, Identifier, IntegerLiteral}
import token.TokenType
import token.TokenType.{EOF, IDENT, SEMICOLON}

trait ParserExpressions {
  parser: Parser =>
  final type PrefixParserFn = () => Option[Expression]
  final val prefixParserFns: Map[TokenType, PrefixParserFn] = Map(
    TokenType.IDENT -> parseIdentifier,
    TokenType.TRUE -> parseBoolean,
    TokenType.FALSE -> parseBoolean,
    TokenType.INT -> parseInteger
  )

  def parseExpression(precedence: ExpressionOrdering): Option[Expression] = {
    val leftExp: Option[Expression] = prefixParserFns.get(cToken.getOrElse(EOFToken).tokenType) match {
      case Some(function: PrefixParserFn) => function()
      case None => None
    }
    leftExp
  }

  def parseExpressionMock(): Some[Expression] = {
    while (cToken.getOrElse(EOFToken).tokenType match {
      case EOF => false
      case SEMICOLON => false
      case _ => true
    }) {
      nextTokens()
    }
    Some(Identifier(EOFToken, ""))
  }

  def parseInteger(): Option[IntegerLiteral] = {
    cToken match {
      case Some(token) if token.literal.toIntOption.isDefined => Some(IntegerLiteral(token, token.literal.toInt))
      case _ => None
    }
  }

  def parseBoolean(): Option[BooleanLiteral] = {
    cToken match {
      case Some(token) if token.literal.toBooleanOption.isDefined => Some(BooleanLiteral(token, token.literal.toBoolean))
      case _ => None
    }
  }

  def parseIdentifier(): Option[Identifier] = {
    cToken match {
      case Some(token) if token.tokenType == IDENT => Some(Identifier(token, token.literal))
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
