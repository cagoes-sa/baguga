package lexer

import org.scalatest.flatspec.AnyFlatSpec
import token.{Token, TokenType}

class LexerSpec extends AnyFlatSpec {
  "nextToken - simple case" should "pass without problems and match all its tokens" in {
    val input = "=+(){},;"
    val expected: Seq[Option[Token]] = Seq(
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.RBRACE, "}"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.EOF, "")
    ).map(Some(_))

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

  "readIdentifier" should "" in {
    val input = "input a a a a a"
    var (_, lexer: Lexer) = Lexer(input).nextToken
    assert(lexer.nextToken._1.contains(Token(TokenType.IDENT, "input")))
  }

  "nextToken - more complex case case" should "pass without problems and match all its tokens" in {
    val input =
      """let five = 5;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);""".stripMargin

    val expected: Seq[Option[Token]] = Seq(
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "five"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.INT, "5"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "ten"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.INT, "10"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "add"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.FUNCTION, "fn"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.IDENT, "x"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.IDENT, "y"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.IDENT, "x"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.IDENT, "y"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.RBRACE, "}"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "result"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.IDENT, "add"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.IDENT, "five"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.IDENT, "ten"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.EOF, "")
    ).map(Some(_))

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
