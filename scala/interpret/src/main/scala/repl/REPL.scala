package repl

import lexer.Lexer

object REPL {
  def run(): Unit = {
    while (true) {
      printf("~ ")
      val line = scala.io.StdIn.readLine()
      val tokenEvaluation = Lexer(line).next
      println(
        tokenEvaluation.getTokens
          .map {
            case Some(token) => token.toString
            case _           => ""
          }
          .mkString("\n")
      )
    }
  }

}
