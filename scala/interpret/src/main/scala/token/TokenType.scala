package token

import enumeratum.{Enum, EnumEntry}

sealed trait TokenType extends EnumEntry

object TokenType extends Enum[TokenType] {
  val values: IndexedSeq[TokenType] = findValues
  object ILLEGAL extends TokenType { override def toString = "ILLEGAL" }
  object EOF extends TokenType { override def toString = "EOF" }

  // Identifiers + literals
  object IDENT extends TokenType { override def toString = "IDENT" }
  object INT extends TokenType { override def toString = "INT" }

  // Operators
  object ASSIGN extends TokenType { override def toString = "=" }
  object PLUS extends TokenType { override def toString = "+" }

  // Delimiters
  object COMMA extends TokenType { override def toString = "," }
  object SEMICOLON extends TokenType { override def toString = ";" }

  object LPAREN extends TokenType { override def toString = "(" }
  object RPAREN extends TokenType { override def toString = ")" }
  object LBRACE extends TokenType { override def toString = "{" }
  object RBRACE extends TokenType { override def toString = "}" }

  // Keywords
  object FUNCTION extends TokenType { override def toString = "FUNCTION" }
  object LET extends TokenType { override def toString = "LET" }
}

