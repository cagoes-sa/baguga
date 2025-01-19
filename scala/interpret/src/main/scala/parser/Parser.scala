package parser

import com.typesafe.scalalogging.Logger
import lexer.Lexer
import parser.Parser.EOFToken
import parser.ast.expressions.ExpressionOrdering.Lowest
import parser.ast.statements.{ExpressionStatement, LetStatement, ReturnStatement}
import parser.ast.{Program, Statement}
import token.TokenType._
import token.{Token, TokenType}

case class Parser(lexer: Lexer) extends ParserDebugger
  with ParserErrors
  with ParserExpressions {
  val tokens: Seq[Seq[Token]] = {
    lexer.next.getTokens.collect { case Some(token) => token }.sliding(2).toSeq
  }

  var iteratorCounter: Int = 0

  def cToken: Option[Token] = if (iteratorCounter < tokens.length) tokens(iteratorCounter).headOption else None

  def pToken: Option[Token] = if (iteratorCounter < tokens.length) tokens(iteratorCounter).tail.headOption else None

  def expectPeek(tokenType: TokenType): Boolean = {
    pToken match {
      case Some(Token(t, _)) if t == tokenType =>
        nextTokens()
        true
      case _ =>
        peekError(tokenType)
        false
    }
  }

  def nextTokens(): Unit = {
    logger.whenDebugEnabled {
      debugTokens()
    }
    iteratorCounter += 1
  }

  def parseProgram(): Program = {
    var programStatements = Seq.empty[Statement]
    while (cToken.getOrElse(EOFToken).tokenType != EOF) {
      val statement = cToken match {
        case Some(Token(LET, _)) => parseLetStatement()
        case Some(Token(RETURN, _)) => parseReturnStatement()
        case _ =>
          parseExpressionsStatement()
      }
      programStatements ++= Seq(statement).flatten
      nextTokens()
    }
    Program(programStatements)
  }

  def parseExpressionsStatement(): Option[ExpressionStatement] = {
    (cToken, parseExpression(Lowest)) match {
      case (Some(token), Some(expression)) =>
        pToken match {
          case Some(Token(SEMICOLON, _)) =>
            nextTokens()
            Some(ExpressionStatement(token, Some(expression)))
          case _ =>
            Some(ExpressionStatement(token, Some(expression)))
        }
      case _ => None
    }
  }

  def parseLetStatement(): Option[LetStatement] = {
    val token = cToken.get
    val identifier = parseIdentifierPeak()
    nextTokens()
    (cToken, identifier) match {
      case (Some(Token(ASSIGN, _)), Some(identifier)) =>
        parseExpressionMock() match {
          case Some(value) =>
            Some(LetStatement(token, identifier, value))
          case _ => None
        }
      case _ => None
    }
  }

  def parseReturnStatement(): Option[ReturnStatement] = {
    val token = cToken.get
    nextTokens()
    parseExpressionMock() match {
      case Some(value) =>
        Some(ReturnStatement(token = token, returnValue = value))
      case _ =>
        None
    }
  }


}



object Parser {
  final val EOFToken = Token(EOF, EOF.toString)
}
