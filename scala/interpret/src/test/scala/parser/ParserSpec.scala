package parser

import errors.ParserError
import lexer.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import parser.ast.{Program, Statement}
import token.Token
import token.TokenType._

class ParserSpec extends AnyFlatSpec with ParserTestUtils {
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

  "LetStatement" should "work" in {
    val input =
      """
        let x = 5;
        let y = 10;
        let foobar = 838383;""".stripMargin
    val lexer = Lexer(input).next
    val parser = Parser(lexer)
    val program = parser.parseProgram
    val expectedLength = 3
    val expectedIdentifiers = Seq("x", "y", "foobar")
    program match {
      case (program: Program, errors: Seq[ParserError]) =>
        if (errors.nonEmpty) {
          println("Got the following errors: ")
          println(errors.map(_.message).mkString("\n"))
        }
        if (program.statements.length != expectedLength) {
          fail(
            s"Program contained only ${program.statements.length} statements, should be $expectedLength"
          )
        }
        program.statements.zip(expectedIdentifiers).map {
          case (statement: Statement, identifier: String) =>
            testLetStatement(statement, identifier)
        }
      case _ => fail("parse program returned none")
    }
  }

  "ReturnStatement" should "work" in {
    val input =
      """
      return 5;
      return 10;
      return 993322;""".stripMargin
    val lexer = Lexer(input).next
    val parser = Parser(lexer)
    val program = parser.parseProgram
    val expectedLength = 3
    program match {
      case (program: Program, errors: Seq[ParserError]) =>
        if (errors.nonEmpty) {
          println("Got the following errors: ")
          println(errors.map(_.message).mkString("\n"))
        }
        if (program.statements.length != expectedLength) {
          fail(
            s"Program contained only ${program.statements.length} statements, should be $expectedLength"
          )
        }
        assert(!program.statements.exists(_.tokenLiteral != "return"))
      case _ => fail("parse program returned none")
    }
  }

}
