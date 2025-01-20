package parser

import errors.ParserError
import parser.Parser.EOFToken
import parser.ast.Expression
import parser.ast.expressions.ExpressionOrdering._
import parser.ast.expressions._
import token.TokenType.{EOF, IDENT, SEMICOLON}
import token.{Token, TokenType}

trait ParserExpressions {
  parser: Parser with ParserErrors =>
  final type PrefixParserFn = () => Option[Expression]
  final type InfixParserFn = Expression => Option[Expression]
  final val prefixParserFns: Map[TokenType, PrefixParserFn] = Map(
    TokenType.IDENT -> parseIdentifier,
    TokenType.TRUE -> parseBoolean,
    TokenType.FALSE -> parseBoolean,
    TokenType.INT -> parseInteger,
    TokenType.BANG -> parsePrefixExpression,
    TokenType.MINUS -> parsePrefixExpression
  )
  final val infixParserFns: Map[TokenType, InfixParserFn] = Map(
    (TokenType.PLUS, parseInfixExpression),
    (TokenType.MINUS, parseInfixExpression),
    (TokenType.SLASH, parseInfixExpression),
    (TokenType.ASTERISK, parseInfixExpression),
    (TokenType.EQ, parseInfixExpression),
    (TokenType.NOT_EQ, parseInfixExpression),
    (TokenType.LT, parseInfixExpression),
    (TokenType.GT, parseInfixExpression),
  )

  def currPrecedence: ExpressionOrdering = precedence.getOrElse(cToken.getOrElse(EOFToken).tokenType, Lowest)

  def peekPrecedence: ExpressionOrdering = precedence.getOrElse(pToken.getOrElse(EOFToken).tokenType, Lowest)

  final val precedence: Map[TokenType, ExpressionOrdering] = Map(
    TokenType.EQ -> Equal,
    TokenType.NOT_EQ -> Equal,
    TokenType.LT -> LessGreater,
    TokenType.GT -> LessGreater,
    TokenType.PLUS -> Sum,
    TokenType.MINUS -> Sum,
    TokenType.SLASH -> Product,
    TokenType.ASTERISK -> Product
  )

  def withNoPrefixParserFoundFor(token: Option[Token]): Unit = {
    parser.errors ++= Seq(ParserError(s"No prefix parser function found for $token"))
  }


  def parseExpression(precedence: ExpressionOrdering): Option[Expression] = {
    logger.debug("On parse expression")
    var leftExp: Option[Expression] = prefixParserFns.get(cToken.getOrElse(EOFToken).tokenType) match {
      case Some(function: PrefixParserFn) => function()
      case None =>
        withNoPrefixParserFoundFor(cToken)
        None
    }
    while ((pToken.getOrElse(EOFToken).tokenType != SEMICOLON) && (precedence < peekPrecedence)) {
      logger.debug("Passing on loop")
      logger.debug(s"Expression in ${leftExp.getOrElse(Identifier(EOFToken, "")).toString}")
      infixParserFns.get(pToken.getOrElse(EOFToken).tokenType) match {
        case Some(function: InfixParserFn) =>
          leftExp = leftExp match {
            case Some(leftExp) =>
              logger.debug("Going to infix")
              nextTokens()
              function(leftExp)
            case None => None
          }
        case None =>
      }
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

  def parseInfixExpression(leftExpression: Expression): Option[InfixExpression] = {
    cToken match {
      case Some(token) =>
        logger.debug("On infix")
        val operator = token.literal
        val precedence = currPrecedence
        nextTokens()
        parser.parseExpression(precedence) match {
          case Some(rightExpression) => Some(InfixExpression(token, operator, leftExpression, rightExpression))
          case _ => None
        }
      case None => None
    }
  }

  def parsePrefixExpression(): Option[PrefixExpression] = {
    cToken match {
      case Some(token) =>
        val operator = token.literal
        nextTokens()
        parser.parseExpression(Prefix) match {
          case Some(rightExpression) => Some(PrefixExpression(token, operator, rightExpression))
          case _ => None
        }
      case _ => None
    }

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
