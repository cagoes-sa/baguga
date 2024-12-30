package parser

import lexer.Lexer
import token.Token

case class Parser(lexer: Lexer) {
  lazy val tokenIterator: Iterator[Seq[Option[Token]]] =
    lexer.getTokens.sliding(2).withPadding(None)
  def getTokenPointers: (Option[Token], Option[Token]) =
    tokenIterator.nextOption() match {
      case Some(Seq(current, peak)) => (current, peak)
      case None                     => (None, None)
    }

}
