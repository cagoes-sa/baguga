package parser

import org.scalatest.Assertions.fail
import parser.ast.Statement
import parser.ast.statements.LetStatement

trait ParserTestUtils {
  def testLetStatement(s: Statement, expectedName: String): Unit = {
    assert(
      s.tokenLiteral == "let",
      s"Statement literal is not let!, got ${s.tokenLiteral}"
    )
    s match {
      case LetStatement(_, identifier, _) =>
        assert(
          identifier.tokenLiteral == expectedName,
          s"Identifier with unexpected token literal: $identifier"
        )
        assert(
          identifier.value == expectedName,
          s"Identifier with unexpected value: $identifier"
        )
      case _ => fail(s"S is not let statement!, got $s")
    }
  }

}
