package lexer

import org.scalatest.Assertions.fail
import token.Token

object LexerTestInstance {
  def test(input: String, expectedTokens: Seq[Token]): Unit = {
    val expected: Seq[Option[Token]] = expectedTokens.map(Some(_))

    val lexer: Lexer = Lexer(input)

    expected
      .zip(lexer.getTokens)
      .foreach {
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
