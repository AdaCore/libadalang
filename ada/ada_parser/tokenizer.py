from compiled_types import Token


class NumLit(Token):
    quex_token_name = 'NUMBER'


class StringLit(Token):
    quex_token_name = 'STRING'


class Lbl(Token):
    quex_token_name = 'LABEL'


class Id(Token):
    quex_token_name = 'IDENTIFIER'


class CharLit(Token):
    quex_token_name = 'CHAR'
