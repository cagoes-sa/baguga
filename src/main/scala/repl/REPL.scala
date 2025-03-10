package repl

import evaluator.Evaluator
import lexer.Lexer
import parser.Parser

object REPL {
  def run(): Unit = {
    val eval = Evaluator("__global__")
    while (true) {
      printf("~ ")
      val line = scala.io.StdIn.readLine()
      val tokenEvaluation = Lexer(line)
      val parser = Parser(tokenEvaluation)
      val program = parser.parseProgram()
      if (parser.errors.nonEmpty) {
        println("Parse Errors: ")
        println(parser.errors.mkString("\n\t"))
      } else {
        eval.evaluate(program) match {
          case Some(value) =>
          case _           => println("Evaluation error!")
        }
      }
    }
  }

}
