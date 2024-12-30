package parser.ast

import lexer.Lexer
import token.Token

case class TokenReader(current: Option[Token], peek: Option[Token])

case class Parser(lexer: Lexer) {
  def tokenIterator: Iterator[Option[Token]] = lexer.getTokens
  def tokenReader: TokenReader =
    (tokenIterator.nextOption(), tokenIterator.nextOption()) match {
      case (Some(optionT1), Some(optionT2)) => TokenReader(optionT1, optionT2)
      case (Some(optionT1), None)           => TokenReader(optionT1, None)
      case (None, _)                        => TokenReader(None, None)
    }

}
