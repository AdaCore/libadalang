from langkit.lexer import (
    Alt, Case, Lexer, LexerToken, Literal, NoCaseLit, Pattern, TokenFamily,
    WithSymbol, WithText, WithTrivia
)


class Token(LexerToken):
    Identifier = WithSymbol()
    All = WithText()

    # Keywords
    Abort = WithText()
    Else = WithText()
    New = WithText()
    Return = WithText()
    Abs = WithText()
    Elsif = WithText()
    Not = WithText()
    Reverse = WithText()
    End = WithText()
    Null = WithSymbol()
    Accept = WithText()
    Entry = WithText()
    Select = WithText()
    Access = WithText()
    Exception = WithText()
    Of = WithText()
    Separate = WithText()
    Exit = WithText()
    Or = WithText()
    Others = WithText()
    Subtype = WithText()
    And = WithText()
    For = WithText()
    Out = WithText()
    Array = WithText()
    Function = WithText()
    At = WithText()
    Generic = WithText()
    Package = WithText()
    Task = WithText()
    Begin = WithText()
    Goto = WithText()
    Pragma = WithText()
    Terminate = WithText()
    Body = WithText()
    Private = WithText()
    Then = WithText()
    If = WithText()
    Procedure = WithText()
    Type = WithText()
    Case = WithText()
    In = WithText()
    Constant = WithText()
    Is = WithText()
    Raise = WithText()
    Use = WithText()
    Declare = WithText()
    Range = WithText()
    Delay = WithText()
    Limited = WithText()
    Record = WithText()
    When = WithText()
    Delta = WithText()
    Loop = WithText()
    Rem = WithText()
    While = WithText()
    Digits = WithText()
    Renames = WithText()
    Do = WithText()
    Mod = WithText()
    Xor = WithText()

    # Punctuation
    ParClose = WithText()
    ParOpen = WithText()
    BrackClose = WithText()
    BrackOpen = WithText()
    Semicolon = WithText()
    Colon = WithText()
    Comma = WithText()
    Doubledot = WithText()
    Dot = WithText()
    Diamond = WithText()
    Lte = WithText()
    Gte = WithText()
    Arrow = WithText()
    Equal = WithText()
    Lt = WithText()
    Gt = WithText()
    Plus = WithText()
    Minus = WithText()
    Power = WithText()
    Mult = WithText()
    Amp = WithText()
    Notequal = WithText()
    Divide = WithText()
    Tick = WithText()
    Pipe = WithText()
    Assign = WithText()
    LabelStart = WithText()
    LabelEnd = WithText()
    Target = WithText()

    # String and char literals
    String = WithText()
    Char = WithSymbol()

    With = WithText()
    Decimal = WithText()
    Integer = WithText()

    # Trivia
    Comment = WithTrivia()
    PrepLine = WithTrivia()
    Whitespace = WithTrivia()

    Alphanumericals = TokenFamily(
        Identifier, All, Abort, Else, New, Return, Abs, Elsif, Not, Reverse,
        End, Null, Accept, Entry, Select, Access, Exception, Of, Separate,
        Exit, Or, Others, Subtype, And, For, Out, Array, Function, At,
        Generic, Package, Task, Begin, Goto, Pragma, Terminate, Body, Private,
        Then, If, Procedure, Type, Case, In, Constant, Is, Raise, Use, Declare,
        Range, Delay, Limited, Record, When, Delta, Loop, Rem, While, Digits,
        Renames, Do, Mod, Xor, With, Decimal, Integer
    )


ada_lexer = Lexer(Token)

ada_lexer.add_patterns(
    ('bracket_char', r'(\[\"[0-9a-fA-F]+\"\])'),
    ('p_string', r'\"(\"\"|{bracket_char}|[^\n\"])*\"'),
    ('p_percent_string', r'%(%%|{bracket_char}|[^\n%])*%'),

    ('digit', r"[0-9]"),
    ('extended_digit', r"[0-9a-zA-Z]"),
    ('integer', r"({digit}(_?{digit})*)"),
    ('exponent', r"([eE](\+?|-){integer})"),

    ('decimal_literal', r"{integer}(\.?{integer})?{exponent}?"),
    ('integer_literal', r"{integer}{exponent}?"),

    ('base', r"{integer}"),
    ('based_integer', r"{extended_digit}(_?{extended_digit})*"),

    ('based_decimal_literal',
     r"{base}[#:]{based_integer}(\.{based_integer})?[#:]{exponent}?"),

    ('based_integer_literal',
     r"{base}[#:]{based_integer}[#:]{exponent}?"),

    ('ws', r"[ ]*"),
    # TODO: handle Unicode properties in Langkit and switch back to
    # \p{ID_Start} and \p{ID_Continue}.
    ('identifier', r"[\$_]?(\p{Lu}|\p{Ll}|\p{Lt}|\p{Lm}|\p{Lo}|\p{Nl}"
                   r"|{bracket_char})"
                   r"(\p{Lu}|\p{Ll}|\p{Lt}|\p{Lm}|\p{Lo}|\p{Nl}|\p{Nd}|\p{Mn}"
                   r"|\p{Mc}"
                   r"|_|{bracket_char})*"),
)

rules = [
    # Blanks and trivia
    (Pattern(r"[ \t\r\n\f\v]+"), Token.Whitespace),
    (Pattern(r"--(.?)+"),        Token.Comment),
    (Pattern(r"#(.?)+"),         Token.PrepLine),
]

# Those keywords can also be attributes, which is why they have special
# handling, for example as in A'Access.
for kw_text, kw_token in [
    ("access", Token.Access),
    ("range",  Token.Range),
    ("digits", Token.Digits),
    ("delta",  Token.Delta),
    ("mod",    Token.Mod),
]:
    rules.append(
        Case(NoCaseLit(kw_text),
             Alt(prev_token_cond=(Token.Tick, ),
                 send=Token.Identifier,
                 match_size=len(kw_text)),
             Alt(send=kw_token, match_size=len(kw_text)))
    )


rules += [
    # Keywords
    (NoCaseLit("abort"),     Token.Abort),
    (NoCaseLit("else"),      Token.Else),
    (NoCaseLit("new"),       Token.New),
    (NoCaseLit("return"),    Token.Return),
    (NoCaseLit("abs"),       Token.Abs),
    (NoCaseLit("elsif"),     Token.Elsif),
    (NoCaseLit("not"),       Token.Not),
    (NoCaseLit("reverse"),   Token.Reverse),
    (NoCaseLit("end"),       Token.End),
    (NoCaseLit("null"),      Token.Null),
    (NoCaseLit("accept"),    Token.Accept),
    (NoCaseLit("entry"),     Token.Entry),
    (NoCaseLit("select"),    Token.Select),
    (NoCaseLit("exception"), Token.Exception),
    (NoCaseLit("of"),        Token.Of),
    (NoCaseLit("separate"),  Token.Separate),
    (NoCaseLit("exit"),      Token.Exit),
    (NoCaseLit("or"),        Token.Or),
    (NoCaseLit("all"),       Token.All),
    (NoCaseLit("others"),    Token.Others),
    (NoCaseLit("subtype"),   Token.Subtype),
    (NoCaseLit("and"),       Token.And),
    (NoCaseLit("for"),       Token.For),
    (NoCaseLit("out"),       Token.Out),
    (NoCaseLit("array"),     Token.Array),
    (NoCaseLit("function"),  Token.Function),
    (NoCaseLit("at"),        Token.At),
    (NoCaseLit("generic"),   Token.Generic),
    (NoCaseLit("package"),   Token.Package),
    (NoCaseLit("task"),      Token.Task),
    (NoCaseLit("begin"),     Token.Begin),
    (NoCaseLit("goto"),      Token.Goto),
    (NoCaseLit("pragma"),    Token.Pragma),
    (NoCaseLit("terminate"), Token.Terminate),
    (NoCaseLit("body"),      Token.Body),
    (NoCaseLit("private"),   Token.Private),
    (NoCaseLit("then"),      Token.Then),
    (NoCaseLit("if"),        Token.If),
    (NoCaseLit("procedure"), Token.Procedure),
    (NoCaseLit("type"),      Token.Type),
    (NoCaseLit("case"),      Token.Case),
    (NoCaseLit("in"),        Token.In),
    (NoCaseLit("constant"),  Token.Constant),
    (NoCaseLit("is"),        Token.Is),
    (NoCaseLit("raise"),     Token.Raise),
    (NoCaseLit("use"),       Token.Use),
    (NoCaseLit("declare"),   Token.Declare),
    (NoCaseLit("delay"),     Token.Delay),
    (NoCaseLit("limited"),   Token.Limited),
    (NoCaseLit("record"),    Token.Record),
    (NoCaseLit("when"),      Token.When),
    (NoCaseLit("loop"),      Token.Loop),
    (NoCaseLit("rem"),       Token.Rem),
    (NoCaseLit("while"),     Token.While),
    (NoCaseLit("renames"),   Token.Renames),
    (NoCaseLit("with"),      Token.With),
    (NoCaseLit("do"),        Token.Do),
    (NoCaseLit("xor"),       Token.Xor),

    # Punctuation
    (Literal("("),  Token.ParOpen),
    (Literal(")"),  Token.ParClose),
    (Literal("["),  Token.BrackOpen),
    (Literal("]"),  Token.BrackClose),
    (Literal(";"),  Token.Semicolon),
    (Literal(":"),  Token.Colon),
    (Literal(","),  Token.Comma),
    (Literal(".."), Token.Doubledot),
    (Literal(":="), Token.Assign),
    (Literal("."),  Token.Dot),
    (Literal("<>"), Token.Diamond),
    (Literal("<="), Token.Lte),
    (Literal(">="), Token.Gte),
    (Literal("=>"), Token.Arrow),
    (Literal("="),  Token.Equal),
    (Literal("<"),  Token.Lt),
    (Literal(">"),  Token.Gt),
    (Literal("+"),  Token.Plus),
    (Literal("-"),  Token.Minus),
    (Literal("**"), Token.Power),
    (Literal("*"),  Token.Mult),
    (Literal("&"),  Token.Amp),
    (Literal("/="), Token.Notequal),
    (Literal("/"),  Token.Divide),
    (Literal("'"),  Token.Tick),
    (Literal("|"),  Token.Pipe),
    (Literal("!"),  Token.Pipe),
    (Literal("<<"), Token.LabelStart),
    (Literal(">>"), Token.LabelEnd),
    (Literal("@"),  Token.Target),

    # Literals
    (Pattern('{integer_literal}'),       Token.Integer),
    (Pattern('{based_integer_literal}'), Token.Integer),

    (Pattern('{decimal_literal}'),       Token.Decimal),
    (Pattern('{based_decimal_literal}'), Token.Decimal),

    (Pattern('{p_string}'),         Token.String),
    (Pattern('{p_percent_string}'), Token.String),

    # Identifiers
    (Pattern('{identifier}'), Token.Identifier),

    (Pattern("'{bracket_char}'"), Token.Char),

    # Attribute vs character literal quirk: A character literal is match via
    # '.'. However, this sequence of characters can happen in other cases, like
    # a qualified expression with a char as parameter: A'Class'('b'). In those
    # cases, we need to send the tick token, rather than the char token.

    Case(Pattern("'.'"),
         Alt(prev_token_cond=(Token.Identifier, ),
             send=Token.Tick,
             match_size=1),
         Alt(send=Token.Char, match_size=3)),
]

ada_lexer.add_rules(*rules)

ada_lexer.add_spacing((Token.Alphanumericals, Token.Alphanumericals))
ada_lexer.add_newline_after(Token.Comment, Token.PrepLine)
