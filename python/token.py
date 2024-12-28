class TokenType(str)

class Token:
    def __init__(self, type_: TokenType, literal: str):
        self.type = type_
        self.literal = literal
