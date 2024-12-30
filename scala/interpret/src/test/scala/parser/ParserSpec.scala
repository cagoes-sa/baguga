package parser

import lexer.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import token.Token
import token.TokenType._

class ParserSpec extends AnyFlatSpec {
  "tokenReader" should "iterate the lexer if each call" in {
    def input = "let x = 5;"
    val lexer = Lexer(input).next
    val parser = Parser(lexer)
    val expectedIterations = Seq(
      (Some(Token(LET, "let")), Some(Token(IDENT, "x"))),
      (Some(Token(IDENT, "x")), Some(Token(ASSIGN, "="))),
      (Some(Token(ASSIGN, "=")), Some(Token(INT, "5"))),
      (Some(Token(INT, "5")), Some(Token(SEMICOLON, ";"))),
      (Some(Token(SEMICOLON, ";")), Some(Token(EOF, "")))
    )

    expectedIterations.map { expected =>
      parser.getTokenPointers match {
        case (current, peak) =>
          assert(expected._1 == current && expected._2 == peak)
      }
    }
  }

}
