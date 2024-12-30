package parser

import lexer.Lexer
import parser.ast.expressions.Identifier
import parser.ast.statements.LetStatement
import parser.ast.{Program, Statement}
import token.{Token, TokenType}

import scala.annotation.tailrec

case class Parser(lexer: Lexer) {
  lazy val tokenIterator: Iterator[Seq[Option[Token]]] =
    lexer.getTokens.sliding(2).withPadding(None)
  def getTokenPointers: (Option[Token], Option[Token]) =
    tokenIterator.nextOption() match {
      case Some(Seq(current, peak)) => (current, peak)
      case None                     => (None, None)
    }

  // Temporary function while no evaluation exists
  private def waitUntil(t: TokenType): Option[(Token, Token)] =
    getTokenPointers match {
      case (None, None) =>
        None
      case (Some(currentToken), Some(peakToken)) if peakToken.tokenType == t =>
        getTokenPointers
        Some((currentToken, peakToken))
      case _ => waitUntil(t)

    }

  def parseLetStatement(
      c: Token,
      optionP: Option[Token]
  ): Option[LetStatement] = {
    val token = c
    val identifier = optionP match {
      case Some(identifierToken)
          if identifierToken.tokenType == TokenType.IDENT =>
        Some(Identifier(identifierToken, identifierToken.literal))
      case _ => None
    }

    getTokenPointers match {
      case (_, Some(assignToken))
          if assignToken.tokenType == TokenType.ASSIGN =>
        waitUntil(TokenType.SEMICOLON) match {
          case None => None
          case Some((ct, pt)) =>
            Some(LetStatement(token, identifier.get, identifier.get))
        }

      case _ => None

    }
  }

  def parseStatement: Option[Statement] = {
    val (currentToken: Option[Token], peakToken: Option[Token]) =
      getTokenPointers
    (currentToken, peakToken) match {
      case (Some(c), _) if c.tokenType == TokenType.EOF => None
      case (Some(c), optionP) =>
        println("c: ", c)
        c.tokenType match {
          case TokenType.LET => parseLetStatement(c, optionP)
          case _             => None
        }
      case (None, None) => None
    }
  }

  def parseProgram: Option[Program] = {
    @tailrec
    def parseProgramIteration(
        program: Program = Program(Seq.empty[Statement])
    ): Program = {
      parseStatement match {
        case None => program
        case Some(statement) =>
          println("iteration")
          parseProgramIteration(Program(program.statements :+ statement))
      }
    }
    Some(parseProgramIteration())
  }

}
