package lexer

import token.{Token, TokenType}

case class Lexer(input: String, position: Int = -1, ch: Byte = 0) {

  def readPosition: Int = position + 1
  def readChar: Byte = {
    if (readPosition >= input.length) 0 else input(readPosition).toByte
  }

  def readIdentifier: (String, Lexer) = {
    if (isLetter(ch)) {
      val readIdentifierNext = next.readIdentifier
      (ch.toChar.toString ++ readIdentifierNext._1, readIdentifierNext._2)
    } else {
      ("", this)
    }
  }

  def readDigit: (String, Lexer) = {
    if (isDigit(ch)) {
      val readIdentifierNext = next.readDigit
      (ch.toChar.toString ++ readIdentifierNext._1, readIdentifierNext._2)
    } else {
      ("", this)
    }
  }

  def next: Lexer = Lexer(input, readPosition, readChar)

  final val identifierLookupTable: Map[String, TokenType] = Map(
    "fn" -> TokenType.FUNCTION,
    "let" -> TokenType.LET
  )
  def lookUpIdentifier(identifier: String): TokenType = {
    identifierLookupTable.getOrElse(identifier, TokenType.IDENT)
  }

  def isWhitespace(c: Byte): Boolean = {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
  }

  def nextToken: (Option[Token], Lexer) = {
    if (isWhitespace(ch)) {
      next.nextToken
    } else {
      ch match {
        case '=' =>
          (Some(Token(TokenType.ASSIGN, ch.toChar.toString)), next)
        case ';' =>
          (Some(Token(TokenType.SEMICOLON, ch.toChar.toString)), next)
        case '(' =>
          (Some(Token(TokenType.LPAREN, ch.toChar.toString)), next)
        case ')' =>
          (Some(Token(TokenType.RPAREN, ch.toChar.toString)), next)
        case ',' =>
          (Some(Token(TokenType.COMMA, ch.toChar.toString)), next)
        case '+' =>
          (Some(Token(TokenType.PLUS, ch.toChar.toString)), next)
        case '{' =>
          (Some(Token(TokenType.LBRACE, ch.toChar.toString)), next)
        case '}' =>
          (Some(Token(TokenType.RBRACE, ch.toChar.toString)), next)
        case 0 =>
          (Some(Token(TokenType.EOF, "")), next)
        case ch if isDigit(ch) =>
          val (identifier: String, nextLexer: Lexer) = readDigit
          (
            Some(
              Token(TokenType.INT, identifier)
            ),
            nextLexer
          )
        case ch if isLetter(ch) =>
          val (identifier: String, nextLexer: Lexer) = readIdentifier
          (
            Some(
              Token(lookUpIdentifier(identifier), identifier)
            ),
            nextLexer
          )
        case _ => (Some(Token(TokenType.ILLEGAL, "")), next)
      }
    }

  }

  def isLetter(ch: Byte): Boolean = {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
  }

  def isDigit(ch: Byte): Boolean = {
    '0' <= ch && ch <= '9'
  }

}
