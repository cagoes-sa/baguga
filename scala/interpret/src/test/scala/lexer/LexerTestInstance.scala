package lexer

import org.scalatest.Assertions.fail
import token.{Token, TokenType}

object LexerTestInstance {
  def test(input: String, expectedTokens: Seq[Token]): Unit = {
    val expected: Seq[Option[Token]] = expectedTokens.map(Some(_))

    var (_, lexer: Lexer) = Lexer(input).nextToken

    expected.foreach { expectedToken =>
      {
        val (resultToken: Option[Token], nlexer: Lexer) = lexer.nextToken
        lexer = nlexer
        (resultToken, expectedToken) match {
          case (Some(t1), Some(t2)) =>
            assert(
              t1.tokenType == t2.tokenType && t1.literal == t2.literal,
              s"expected token $t2 and got $t1"
            )
          case (None, Some(_)) => fail("Expected token and got EOF")
          case (Some(_), None) => fail("Expected EOF and got token")
        }
      }
    }
  }
}
