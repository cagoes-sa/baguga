package parser

import com.typesafe.scalalogging.Logger
import lexer.Lexer
import parser.ast.expressions.Identifier
import parser.ast.statements.LetStatement
import parser.ast.{Expression, Program, Statement}
import token.{Token, TokenType}
import token.TokenType.{ASSIGN, EOF, EQ, IDENT, LET, SEMICOLON}

case class Parser(lexer: Lexer) extends ParserDebugger with ParserErrors {
  val tokens: Seq[Seq[Token]] = {
    lexer.next.getTokens.collect { case Some(token) => token } .sliding(2).toSeq
  }

  private final val EOFToken = Token(EOF, EOF.toString)
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
    iteratorCounter += 1
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
  def parseExpression(): Some[Expression] = ???

  def parseIdentifier(): Option[Identifier] = {
    if (expectPeek(IDENT)) {
      cToken match {
        case Some(token) => Some(Identifier(token, token.literal))
        case _ => None
      }
    } else {
      None
    }
  }

  def parseLetStatement(): Option[LetStatement] = {
    val token = cToken.get
    val identifier = parseIdentifier()
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

  def parseProgram(): Program = {
    var programStatements  = Seq.empty[Statement]
    while (cToken.getOrElse(EOFToken).tokenType != EOF) {
      val statement = cToken match {
        case Some(token) if token.tokenType == LET =>
          parseLetStatement()
        case _ =>
          None
      }
      programStatements ++= Seq(statement).flatten
      nextTokens()
    }
    Program(programStatements)
  }

}

trait ParserDebugger { parser: Parser =>
  val logger: Logger = Logger(parser.getClass)
  def debugTokens(): Unit = {
    logger.whenDebugEnabled{
      logger.debug(s"Current [$cToken] - [$pToken]O")
    }
  }
}
