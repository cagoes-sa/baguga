package evaluator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EvaluatorSpec extends AnyFlatSpec with EvaluatorMatchers {

  "Literals" should "be correctly valuated" in {
    "5" should beEvaluatedWithType(ObjectType.Integer)
    "5" should beEqualTo(BigInt(5))
    "10" should beEvaluatedWithType(ObjectType.Integer)
    "10" should beEqualTo(BigInt(10))

    "false" should beEvaluatedWithType(ObjectType.Boolean)
    "false" should beEqualTo(false)
    "true" should beEvaluatedWithType(ObjectType.Boolean)
    "true" should beEqualTo(true)

    "\"hey\"" should beEqualTo("hey")
    "\" you need to pick your afro \\\"daddy\\\" \"" should beEqualTo(
      " you need to pick your afro \\\"daddy\\\" "
    )

    "[1, 2, \"hey\"]" should beInspectedInto("[1,2,hey]")
    "[1, 2,[1,2,3,\"hey\"]]" should beInspectedInto("[1,2,[1,2,3,hey]]")
    "[]" should beInspectedInto("[]")
    "[[[]]]" should beInspectedInto("[[[]]]")
    "[[],[]]" should beInspectedInto("[[],[]]")

  }

  "Prefix expressions" should "be correctly valuated" in {
    "!false" should beEqualTo(true)
    "!true" should beEqualTo(false)
    "!5" should beEqualTo(false)

    "-5" should beEqualTo(BigInt(-5))
    "-232131" should beEqualTo(-232131)
    "-false" should failWithMessage("unknown operator: -BOOLEAN")
  }

  "Infix expressions" should "be correctly valuated" in {
    "1 + 5" should beEqualTo(6)
    "50 / 2 * 2 + 10" should beEqualTo(60)
    "2 * (5 + 10)" should beEqualTo(30)
    "3 * 3 * 3 + 10" should beEqualTo(37)
    "3 * (3 * 3) + 10" should beEqualTo(37)
    "(5 + 10 * 2 + 15 / 3) * 2 + -10" should beEqualTo(50)

    "true" should beEqualTo(true)
    "false" should beEqualTo(false)
    "1 < 2" should beEqualTo(true)
    "1 > 2" should beEqualTo(false)
    "1 < 1" should beEqualTo(false)
    "1 > 1" should beEqualTo(false)
    "1 == 1" should beEqualTo(true)
    "1 != 1" should beEqualTo(false)
    "1 == 2" should beEqualTo(false)
    "1 != 2" should beEqualTo(true)

    "true == true" should beEqualTo(true)
    "false == false" should beEqualTo(true)
    "true == false" should beEqualTo(false)
    "true != false" should beEqualTo(true)
    "false != true" should beEqualTo(true)
    "(1 < 2) == true" should beEqualTo(true)
    "(1 < 2) == false" should beEqualTo(false)
    "(1 > 2) == true" should beEqualTo(false)
    "(1 > 2) == false" should beEqualTo(true)
  }

  "Conditional operators" should "be evaluated correctly" in {
    "if (true) { 10 }" should beEqualTo(10)
    "if (false) { 10 }" should beEqualTo(value = None)
    "if (1) { 10 }" should beEqualTo(10)
    "if (1 < 2) { 10 }" should beEqualTo(10)
    "if (1 > 2) { 10 }" should beEqualTo(value = None)
    "if (1 > 2) { 10 } else { 20 }" should beEqualTo(20)
    "if (1 < 2) { 10 } else { 20 }" should beEqualTo(10)
  }

  "Return statements" should "be evaluated correctly" in {
    "return 10;" should beEqualTo(10)
    "return 10; 9;" should beEqualTo(10)
    "return 2 * 5; 9;" should beEqualTo(10)
    "9; return 2 * 5; 9;" should beEqualTo(10)
    """
      |if (10 > 1) {
      |  if (10 > 1) {
      |    return 10;
      |  }
      |
      |  return 1;
      |}
      |""" should beEqualTo(10)
  }

  "Error Handling" should "work" in {
    "5 + true;" should failWithMessage("type mismatch: INTEGER + BOOLEAN")
    "5 + true; 5;" should failWithMessage("type mismatch: INTEGER + BOOLEAN")
    "-true" should failWithMessage("unknown operator: -BOOLEAN")
    "true + false;" should failWithMessage("type mismatch: BOOLEAN + BOOLEAN")
    "if (10 > 1) ( true + false; )" should failWithMessage(
      "type mismatch: BOOLEAN + BOOLEAN"
    )
    "5; true + false; 5" should failWithMessage(
      "type mismatch: BOOLEAN + BOOLEAN"
    )
    """
      |if (10 > 1) {
      |  if (10 > 1) {
      |    return true + false;
      |  }
      |
      |  return 1;
      |}
      |""".stripMargin should failWithMessage(
      "type mismatch: BOOLEAN + BOOLEAN"
    )
  }

  "Array operations" should "work" in {
    "[] + [1];" should beInspectedInto("[1]")
    "[1] + [1, 2, 3];" should beInspectedInto("[1,1,2,3]")
    "[1,2,3] == [1, 2, 3];" should beInspectedInto("true")
    "[1,2,3] == [1, 3, 2];" should beInspectedInto("false")
    """
      |let addString = fn(array, string) {
      | return array + [string];
      |};
      |let array = [1,2,3];
      |let string = "hey";
      |addString(array, string);
      |""".stripMargin should beInspectedInto("[1,2,3,hey]")
  }

  "Let Statements" should "work" in {
    "let a = 5; a;" should beEqualTo(5)
    "let a = 5 * 5; a;" should beEqualTo(25)
    "let a = 5; let b = a; let c = false; b; a + b;" should beEqualTo(10)
    "let a = 5; let b = a; let c = a + b + 5; c;" should beEqualTo(15)
  }

  "While Statements" should "work" in {
    """
      |let c = 0;
      |let a = [];
      |while ( c < 3 ) {
      |    let c = c + 1;
      |    let a = a + [c];
      |}
      |a;
      |""".stripMargin should beInspectedInto("[1,2,3]")

    """
      |let c = 0;
      |let a = [];
      |while ( c < 3 ) {
      |    let c = c + 1;
      |    let a = a + [c];
      |}
      |c;
      |""".stripMargin should beEqualTo(3)

  }

  "Function Statements" should "work" in {
    "let x = fn(y) { return y + 1; }; x(10); " should beEqualTo(11)
    "let x = fn(a, b) { if (a > b) { a } else { b } }; x(2, 1) + x(2, 3); " should beEqualTo(
      5
    )
    "let x = fn (c) { if (c < 10) { c + x(c+1); } else { c; }}; x(0);" should beEqualTo(
      55
    )
    "fn (c) { if (c < 10) { c + 1} else { c; }}(0);" should beEqualTo(1)
  }

  "Builtin Functions" should "run" in {
    "print(10, \"\n\")" should beEqualTo(value = None)

    """
      |let c = 10;
      |while (c > 0) {
      | let c = c - 1;
      | print(c, "\n");
      |};
      |""".stripMargin should beEqualTo(value = None)

  }

  "Baguga Statements" should "run" in {
    "print(10, \"\n\")" should beEqualTo(value = None)

    """
      {
       print(c, a, x);
      } mas antes {
       let c = [a];
      } mas antes {
       {
         let a = 5 + x;
       } mas antes {
         let x = 5;
       } BAGUGA;
      } mas antes {
       let c = 10;
      } BAGUGA
      """.stripMargin should beEqualTo(value = None)

  }
}
