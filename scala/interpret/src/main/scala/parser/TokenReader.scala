package parser

import token.Token

case class TokenReader(current: Option[Token], peek: Option[Token])


