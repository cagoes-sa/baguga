package parser

import com.typesafe.scalalogging.Logger
import lexer.Lexer
import token.Token

case class Parser(lexer: Lexer) extends ParserDebugger {
  val tokens: Seq[Seq[Token]] = {
    lexer.next.getTokens.collect { case Some(token) => token } .sliding(2).toSeq
  }

  var iteratorCounter: Int = 0
  def cToken: Option[Token] = if (iteratorCounter < tokens.length) tokens(iteratorCounter).headOption else None
  def pToken: Option[Token] = if (iteratorCounter < tokens.length) tokens(iteratorCounter).tail.headOption else None

  def nextTokens(): Unit = {
    iteratorCounter += 1
  }

}

trait ParserDebugger { parser: Parser =>
  val logger: Logger = Logger(parser.getClass)
  def debugTokens(): Unit = {

    logger.debug(s"Current [$cToken] - [$pToken]")

  }
}
