package parser

import errors.ParserError
import lexer.Lexer
import parser.ast.expressions.{
  ExpressionOrdering,
  Identifier,
  InfixExpression,
  PrefixExpression
}
import parser.ast.statements.{
  ExpressionStatement,
  LetStatement,
  ReturnStatement
}
import parser.ast.{Expression, Program, Statement}
import token.{Token, TokenType}

import scala.annotation.tailrec
import scala.collection.BufferedIterator

case class Parser(lexer: Lexer) {

  lazy val tokenIterator: BufferedIterator[Seq[Option[Token]]] =
    lexer.getTokens.sliding(2).buffered
  def getTokenPointers: (Option[Token], Option[Token]) =
    tokenIterator.headOption match {
      case Some(Seq(current, peak)) =>
        (current, peak)
      case None => (None, None)
    }

  def nextTokenPointers: (Option[Token], Option[Token]) =
    tokenIterator.nextOption() match {
      case Some(Seq(current, peak)) =>
        (current, peak)
      case None => (None, None)
    }

  // Temporary function while no evaluation exists
  private def waitUntil(t: TokenType): Option[(Token, Token)] =
    nextTokenPointers match {
      case (None, None) =>
        None
      case (Some(currentToken), Some(peakToken)) if peakToken.tokenType == t =>
        nextTokenPointers
        Some((currentToken, peakToken))
      case _ => waitUntil(t)

    }

  def parseReturnStatement(
      c: Token,
      optionP: Option[Token]
  ): (Option[ReturnStatement], Seq[ParserError]) = {
    val token = c

    token match {
      case returnToken: Token if returnToken.tokenType == TokenType.RETURN =>
        waitUntil(TokenType.SEMICOLON)
        (
          Some(
            ReturnStatement(
              token,
              new Expression {
                override def tokenLiteral: String = ""

                override def expressionNode(): Unit = {}
                override def string = ""
              }
            )
          ),
          Seq.empty[ParserError]
        )
      case _ =>
        (
          None,
          Seq(ParserError(s"Expected a return statement, got ${c.tokenType}"))
        )
    }

  }

  def parseLetStatement(
      c: Token,
      optionP: Option[Token]
  ): (Option[LetStatement], Seq[ParserError]) = {
    val token = c
    val (identifier, errors) = optionP match {
      case Some(identifierToken)
          if identifierToken.tokenType == TokenType.IDENT =>
        (
          Some(Identifier(identifierToken, identifierToken.literal)),
          Seq.empty[ParserError]
        )
      case Some(otherToken) =>
        (
          None,
          Seq(
            ParserError(
              s"Token $otherToken should be from the identifier type, but got ${otherToken.tokenType}"
            )
          )
        )
    }

    nextTokenPointers match {
      case (_, Some(assignToken))
          if assignToken.tokenType == TokenType.ASSIGN =>
        waitUntil(TokenType.SEMICOLON) match {
          case None => (None, errors)
          case Some((ct, pt)) =>
            (Some(LetStatement(token, identifier.get, identifier.get)), errors)
        }

      case (_, Some(otherToken)) =>
        waitUntil(TokenType.SEMICOLON)
        (
          None,
          errors ++ Seq(
            ParserError(s"Expected an ASSIGN token, but got $otherToken")
          )
        )

    }
  }

  def parseExpression(
      precedence: ExpressionOrdering,
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    println(s"Parse Expression Call - \n\tprecedence ${precedence}")
    println(s"\ttokens ${c} and $optionP")
    val (leftExp, leftExpErrors) =
      ParserFns.prefixParseFns(this).get(c.tokenType) match {
        case Some(fn) => fn(c, optionP)
        case None =>
          (
            None,
            Seq(
              ParserError(
                s"Token type ${c.tokenType} not mapped into expression parser"
              )
            )
          )
      }

    def recursiveInfixValuation(
        tokens: (Token, Token),
        expressionIteration: (Option[Expression], Seq[ParserError])
    ): (Option[Expression], Seq[ParserError]) = {
      println("RecursiveInfixValuation Call")
      val (leftExp, leftExpErrors) = expressionIteration
      val (cur, peek) = tokens
      leftExp match {
        case Some(leftExp) =>
          println(s"\tRecursiveInfix Valuation Iteration: ${leftExp.string}")
          println(s"\tprecedence: $precedence")
          println(
            s"\t\tc: ${cur.tokenType} - ${ParserFns.getPrecedence(cur)} - ${cur.literal}"
          )
          println(
            s"\t\tp: ${peek.tokenType} - ${ParserFns
              .getPrecedence(peek)} - ${peek.literal}"
          )
        case _ =>
      }
      ParserFns.infixParseFns(this).get(peek.tokenType) match {
        case None =>
          (leftExp, leftExpErrors)
        case Some(fn) =>
          leftExp match {
            case Some(leftExp) =>
              fn(leftExp, c, optionP) match {
                case output =>
                  recursiveInfixValuation(
                    nextTokenPointers match {
                      case (Some(a), Some(b)) =>
                        println(s"These are the next tokens: $a $b")
                        (a, b)
                    },
                    output
                  )
              }
            case None =>
              (leftExp, leftExpErrors)
          }
      }
    }
    optionP match {
      case Some(peekToken)
          if !(ParserFns
            .getPrecedence(peekToken) > precedence) =>
        println("\tPrecedence is not bigger, will just go to next tokens ")
        (leftExp, leftExpErrors)
      case _ =>
        println("Am I going to another place?")
        leftExp match {
          case Some(leftExp) =>
            recursiveInfixValuation(
              (c, optionP.get),
              (Some(leftExp), leftExpErrors)
            )
          case None =>
            (None, leftExpErrors)
        }

    }

  }

  def parseExpressionStatement(
      c: Token,
      optionP: Option[Token]
  ): (Option[ExpressionStatement], Seq[ParserError]) = {
    println("-- Expression Statement --")

    parseExpression(ExpressionOrdering.Lowest, c, optionP) match {
      case (Some(expression: Expression), errors: Seq[ParserError]) =>
        optionP match {
          case Some(token) if token.tokenType == TokenType.SEMICOLON =>
            nextTokenPointers
            (Some(ExpressionStatement(c, Some(expression))), errors)
          case Some(token) if expression.isInstanceOf[InfixExpression] =>
            (Some(ExpressionStatement(c, Some(expression))), errors)
          case Some(token) if expression.isInstanceOf[PrefixExpression] =>
            (Some(ExpressionStatement(c, Some(expression))), errors)
          case None => (None, errors)
        }
      case (None, errors: Seq[ParserError]) => (None, errors)
    }
  }

  def parseStatement: (Option[Statement], Seq[ParserError]) = {
    val (currentToken: Option[Token], peakToken: Option[Token]) =
      nextTokenPointers
    (currentToken, peakToken) match {
      case (Some(c), _) if c.tokenType == TokenType.EOF =>
        (None, Seq.empty[ParserError])
      case (Some(c), optionP) =>
        c.tokenType match {
          case TokenType.LET    => parseLetStatement(c, optionP)
          case TokenType.RETURN => parseReturnStatement(c, optionP)
          case _                => parseExpressionStatement(c, optionP)
        }
      case (None, None) => (None, Seq.empty[ParserError])
    }
  }

  def parseProgram: (Program, Seq[ParserError]) = {
    @tailrec
    def parseProgramIteration(
        program: Program = Program(Seq.empty[Statement]),
        errors: Seq[ParserError] = Seq.empty[ParserError]
    ): (Program, Seq[ParserError]) = {
      println(s"Program Iteration: ${program.string}")
      if (tokenIterator.hasNext) {
        parseStatement match {
          case (None, statementErrors) =>
            parseProgramIteration(program, errors ++ statementErrors)
          case (Some(statement), statementErrors) =>
            parseProgramIteration(
              Program(program.statements :+ statement),
              errors ++ statementErrors
            )
        }
      } else {
        (program, errors)
      }
    }
    parseProgramIteration()
  }

}
