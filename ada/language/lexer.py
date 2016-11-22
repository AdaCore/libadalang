from langkit.lexer import (
    Lexer, NoCase, Literal, Pattern, NoText, Ignore, WithText, Case, Alt,
    LexerToken, WithSymbol, WithTrivia
)


class Token(LexerToken):
    Identifier = WithSymbol()
    All = NoText()

    # Keywords
    Abort = NoText()
    Else = NoText()
    New = NoText()
    Return = NoText()
    Abs = NoText()
    Elsif = NoText()
    Not = NoText()
    Reverse = NoText()
    Abstract = NoText()
    End = NoText()
    Null = WithSymbol()
    Accept = NoText()
    Entry = NoText()
    Select = NoText()
    Access = NoText()
    Exception = NoText()
    Of = NoText()
    Separate = NoText()
    Aliased = NoText()
    Exit = NoText()
    Or = NoText()
    Some = NoText()
    Others = NoText()
    Subtype = NoText()
    And = NoText()
    For = NoText()
    Out = NoText()
    Array = NoText()
    Function = NoText()
    Overriding = NoText()
    At = NoText()
    Tagged = NoText()
    Generic = NoText()
    Package = NoText()
    Task = NoText()
    Begin = NoText()
    Goto = NoText()
    Pragma = NoText()
    Terminate = NoText()
    Body = NoText()
    Private = NoText()
    Then = NoText()
    If = NoText()
    Procedure = NoText()
    Type = NoText()
    Case = NoText()
    In = NoText()
    Protected = NoText()
    Constant = NoText()
    Is = NoText()
    Raise = NoText()
    Use = NoText()
    Declare = NoText()
    Range = NoText()
    Delay = NoText()
    Until = NoText()
    Limited = NoText()
    Record = NoText()
    When = NoText()
    Delta = NoText()
    Loop = NoText()
    Rem = NoText()
    While = NoText()
    Digits = NoText()
    Renames = NoText()
    Withs = NoText()
    Do = NoText()
    Mod = NoText()
    Requeue = NoText()
    Xor = NoText()

    # Label
    Label = WithSymbol()

    # Punctuation
    ParClose = NoText()
    ParOpen = NoText()
    Semicolon = NoText()
    Colon = NoText()
    Comma = NoText()
    Doubledot = NoText()
    Dot = NoText()
    Diamond = NoText()
    Lte = NoText()
    Gte = NoText()
    Arrow = NoText()
    Equal = NoText()
    Lt = NoText()
    Gt = NoText()
    Plus = NoText()
    Minus = NoText()
    Power = NoText()
    Mult = NoText()
    Amp = NoText()
    Notequal = NoText()
    Divide = NoText()
    Tick = NoText()
    Pipe = NoText()
    Assign = NoText()

    # String and char literals
    String = WithText()
    Char = WithSymbol()

    With = NoText()
    Decimal = WithText()
    Integer = WithText()

    # Trivia
    Comment = WithTrivia()

ada_lexer = Lexer(Token)

ada_lexer.add_patterns(
    ('bracket_char', r'(\[\"([0-9A-F][0-9A-F]){2,4}\"\])'),
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
    ('identifier', r"(\P{ID_Start}|{bracket_char})"
                   r"(\P{ID_Continue}*|{bracket_char})*"),
)

rules = [
    # Blanks and trivia
    (Pattern(r"[ \t\r\n]+"),                    Ignore()),
    (Pattern(r"--(.?)+"),                       Token.Comment),
]

# Keywords. If a tick appeared right before, this is actually an attribute
# reference, so it's not a keyword.
for kw_text, kw_token in [
    ("abort",        Token.Abort),
    ("else",         Token.Else),
    ("new",          Token.New),
    ("return",       Token.Return),
    ("abs",          Token.Abs),
    ("elsif",        Token.Elsif),
    ("not",          Token.Not),
    ("reverse",      Token.Reverse),
    ("abstract",     Token.Abstract),
    ("end",          Token.End),
    ("null",         Token.Null),
    ("accept",       Token.Accept),
    ("entry",        Token.Entry),
    ("select",       Token.Select),
    ("access",       Token.Access),
    ("exception",    Token.Exception),
    ("of",           Token.Of),
    ("separate",     Token.Separate),
    ("aliased",      Token.Aliased),
    ("exit",         Token.Exit),
    ("or",           Token.Or),
    ("some",         Token.Some),
    ("all",          Token.All),
    ("others",       Token.Others),
    ("subtype",      Token.Subtype),
    ("and",          Token.And),
    ("for",          Token.For),
    ("out",          Token.Out),
    ("array",        Token.Array),
    ("function",     Token.Function),
    ("overriding",   Token.Overriding),
    ("at",           Token.At),
    ("tagged",       Token.Tagged),
    ("generic",      Token.Generic),
    ("package",      Token.Package),
    ("task",         Token.Task),
    ("begin",        Token.Begin),
    ("goto",         Token.Goto),
    ("pragma",       Token.Pragma),
    ("terminate",    Token.Terminate),
    ("body",         Token.Body),
    ("private",      Token.Private),
    ("then",         Token.Then),
    ("if",           Token.If),
    ("procedure",    Token.Procedure),
    ("type",         Token.Type),
    ("case",         Token.Case),
    ("in",           Token.In),
    ("protected",    Token.Protected),
    ("constant",     Token.Constant),
    ("is",           Token.Is),
    ("raise",        Token.Raise),
    ("use",          Token.Use),
    ("declare",      Token.Declare),
    ("range",        Token.Range),
    ("delay",        Token.Delay),
    ("until",        Token.Until),
    ("limited",      Token.Limited),
    ("record",       Token.Record),
    ("when",         Token.When),
    ("delta",        Token.Delta),
    ("loop",         Token.Loop),
    ("rem",          Token.Rem),
    ("while",        Token.While),
    ("digits",       Token.Digits),
    ("renames",      Token.Renames),
    ("with",         Token.With),
    ("do",           Token.Do),
    ("mod",          Token.Mod),
    ("requeue",      Token.Requeue),
    ("xor",          Token.Xor),
]:
    rules.append(
        Case(NoCase(kw_text),
             Alt(prev_token_cond=(Token.Tick, ),
                 send=Token.Identifier,
                 match_size=len(kw_text)),
             Alt(send=kw_token, match_size=len(kw_text)))
    )

rules += [
    # Punctuation
    (Literal("("),                              Token.ParOpen),
    (Literal(")"),                              Token.ParClose),
    (Literal(";"),                              Token.Semicolon),
    (Literal(":"),                              Token.Colon),
    (Literal(","),                              Token.Comma),
    (Literal(".."),                             Token.Doubledot),
    (Literal(":="),                             Token.Assign),
    (Literal("."),                              Token.Dot),
    (Literal("<>"),                             Token.Diamond),
    (Literal("<="),                             Token.Lte),
    (Literal(">="),                             Token.Gte),
    (Literal("=>"),                             Token.Arrow),
    (Literal("="),                              Token.Equal),
    (Literal("<"),                              Token.Lt),
    (Literal(">"),                              Token.Gt),
    (Literal("+"),                              Token.Plus),
    (Literal("-"),                              Token.Minus),
    (Literal("**"),                             Token.Power),
    (Literal("*"),                              Token.Mult),
    (Literal("&"),                              Token.Amp),
    (Literal("/="),                             Token.Notequal),
    (Literal("/"),                              Token.Divide),
    (Literal("'"),                              Token.Tick),
    (Literal("|"),                              Token.Pipe),

    # Literals
    (ada_lexer.patterns.integer_literal,        Token.Integer),
    (ada_lexer.patterns.decimal_literal,        Token.Decimal),
    (ada_lexer.patterns.based_integer_literal,  Token.Integer),
    (ada_lexer.patterns.based_decimal_literal,  Token.Decimal),

    (ada_lexer.patterns.p_string,               Token.String),
    (ada_lexer.patterns.p_percent_string,       Token.String),

    # Identifiers
    (ada_lexer.patterns.identifier,             Token.Identifier),
    (Pattern(r"<<{ws}({identifier})?{ws}>>"),   Token.Label),

    # Attribute vs character literal quirk
    Case(Pattern("'.'"),
         Alt(prev_token_cond=(Token.Identifier, Token.All),
             send=Token.Tick,
             match_size=1),
         Alt(send=Token.Char, match_size=3)),
]

ada_lexer.add_rules(*rules)
