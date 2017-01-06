from langkit.lexer import (
    Lexer, NoCase, Literal, Pattern, Ignore, WithText, Case, Alt,
    LexerToken, WithSymbol, WithTrivia
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
    Abstract = WithText()
    End = WithText()
    Null = WithSymbol()
    Accept = WithText()
    Entry = WithText()
    Select = WithText()
    Access = WithText()
    Exception = WithText()
    Of = WithText()
    Separate = WithText()
    Aliased = WithText()
    Exit = WithText()
    Or = WithText()
    Some = WithText()
    Others = WithText()
    Subtype = WithText()
    And = WithText()
    For = WithText()
    Out = WithText()
    Array = WithText()
    Function = WithText()
    At = WithText()
    Tagged = WithText()
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
    Until = WithText()
    Limited = WithText()
    Record = WithText()
    When = WithText()
    Delta = WithText()
    Loop = WithText()
    Rem = WithText()
    While = WithText()
    Digits = WithText()
    Renames = WithText()
    Withs = WithText()
    Do = WithText()
    Mod = WithText()
    Requeue = WithText()
    Xor = WithText()

    # Label
    Label = WithSymbol()

    # Punctuation
    ParClose = WithText()
    ParOpen = WithText()
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

    # String and char literals
    String = WithText()
    Char = WithSymbol()

    With = WithText()
    Decimal = WithText()
    Integer = WithText()

    # Trivia
    Comment = WithTrivia()
    PrepLine = WithTrivia()

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
    ('identifier', r"\$?(\P{ID_Start}|{bracket_char})"
                   r"(\P{ID_Continue}*|{bracket_char})*"),
)

rules = [
    # Blanks and trivia
    (Pattern(r"[ \t\r\n\r\f]+"),                Ignore()),
    (Pattern(r"--(.?)+"),                       Token.Comment),
    (Pattern(r"#(.?)+"),                        Token.PrepLine),
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
    (Literal("!"),                              Token.Pipe),

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

    (Pattern("'{bracket_char}'"),               Token.Char),

    # Attribute vs character literal quirk
    Case(Pattern("'.'"),
         Alt(prev_token_cond=(Token.Identifier, Token.All),
             send=Token.Tick,
             match_size=1),
         Alt(send=Token.Char, match_size=3)),
]

ada_lexer.add_rules(*rules)
