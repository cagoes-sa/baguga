package lexer

import org.scalatest.flatspec.AnyFlatSpec
import token.{Token, TokenType}

class LexerSpec extends AnyFlatSpec {
  "nextToken" should "" in {
    val input = "=+(){},;"
    val expected: Seq[Option[Token]] = Seq(
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.RBRACE, ")"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.EOF, "")
    ).map(Some(_))

    val lexer = Lexer(input)

    expected.foreach { expectedToken =>
      {
        val resultToken = lexer.nextToken()
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
