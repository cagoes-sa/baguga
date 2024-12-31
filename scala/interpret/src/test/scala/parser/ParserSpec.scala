package parser

import errors.ParserError
import lexer.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import parser.ast.expressions.{Identifier, IntegerLiteral}
import parser.ast.statements.ExpressionStatement
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

  "ExpressionParser - identifiers" should "Be correctly parsed" in {
    val input = "foobar;"

    val l = Lexer(input).next
    val p = Parser(l)
    val (program: Program, errors: Seq[ParserError]) = p.parseProgram

    assert(program.statements.length == 1)
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: Identifier) =>
            assert(ident.value == "foobar")
            assert(ident.tokenLiteral == "foobar")
          case _ => fail("Statement expression should be identifier")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

  "ExpressionParser - integer literals" should "Be correctly parsed" in {
    val input = "420;"

    val l = Lexer(input).next
    val p = Parser(l)
    val (program: Program, errors: Seq[ParserError]) = p.parseProgram

    assert(program.statements.length == 1)
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: IntegerLiteral) =>
            assert(ident.value == 420)
            assert(ident.tokenLiteral == "420")
          case _ => fail("Statement expression should be IntegerLiteral")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

}
