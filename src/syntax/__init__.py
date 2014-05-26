from utils import StructEq
from combinators import List, TokClass, Grammar, ASTNode, Field, TokenType, Tok
from tokenizer import Id, CharLit, NumLit, Kw, StringLit, Token

ada_grammar = Grammar()
A = ada_grammar


class ToImplementNode(ASTNode):
    fields = []
    pass

