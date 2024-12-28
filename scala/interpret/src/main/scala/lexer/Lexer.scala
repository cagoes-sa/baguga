package lexer

import token.{Token, TokenType}

case class Lexer(input: String, position: Int = -1, ch: Byte = 0) {

  def readPosition: Int = position + 1
  def readChar: Lexer = {
    val newCh: Byte =
      if (readPosition >= input.length) 0 else input(readPosition).toByte
    Lexer(input, readPosition, newCh)
  }
  def nextToken(): Option[Token] = {
    ch match {
      case '=' =>
        Some(Token(TokenType.ASSIGN, ch.toChar.toString))
      case ';' =>
        Some(Token(TokenType.SEMICOLON, ch.toChar.toString))
      case '(' =>
        Some(Token(TokenType.LPAREN, ch.toChar.toString))
      case ')' =>
        Some(Token(TokenType.RPAREN, ch.toChar.toString))
      case ',' =>
        Some(Token(TokenType.COMMA, ch.toChar.toString))
      case '+' =>
        Some(Token(TokenType.PLUS, ch.toChar.toString))
      case '{' =>
        Some(Token(TokenType.LBRACE, ch.toChar.toString))
      case '}' =>
        Some(Token(TokenType.RBRACE, ch.toChar.toString))
      case 0 =>
        Some(Token(TokenType.EOF, ""))
      case _ => None
    }
  }

}
