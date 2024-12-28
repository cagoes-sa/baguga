class TokenType(str):
    ILLEGAL = "ILLEGAL"
    EOF     = "EOF"

    # Identifiers + literals
    IDENT = "IDENT" # add, foobar, x, y, ...
    INT   = "INT"   # 1343456

    # Operators
    ASSIGN   = "="
    PLUS     = "+"

    # Delimiters
    COMMA     = ","
    SEMICOLON = ";"

    LPAREN = "("
    RPAREN = ")"
    LBRACE = "{"
    RBRACE = "}"

    # Keywords
    FUNCTION = "FUNCTION"
    LET      = "LET"


class Token:
    def __init__(self, type_: TokenType, literal: str):
        self.type = type_
        self.literal = literal
