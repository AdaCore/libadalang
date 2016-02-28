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
    Synchronized = NoText()
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
    Interface = NoText()
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
    Par_close = NoText()
    Par_open = NoText()
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
    Number = WithText()

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
    ('base', r"{integer}"),
    ('based_integer', r"{extended_digit}(_?{extended_digit})*"),
    ('identifier', r"(\P{ID_Start}|{bracket_char})"
                   r"(\P{ID_Continue}*|{bracket_char})*"),
    ('based_literal',
     r"{base}[#:]{based_integer}(\.{based_integer})?[#:]{exponent}?"),
    ('ws', r"[ ]*"),
)

ada_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"),                    Ignore()),
    (Pattern(r"--(.?)+"),                       Token.Comment),
    (NoCase("abort"),                           Token.Abort),
    (NoCase("else"),                            Token.Else),
    (NoCase("new"),                             Token.New),
    (NoCase("return"),                          Token.Return),
    (NoCase("abs"),                             Token.Abs),
    (NoCase("elsif"),                           Token.Elsif),
    (NoCase("not"),                             Token.Not),
    (NoCase("reverse"),                         Token.Reverse),
    (NoCase("abstract"),                        Token.Abstract),
    (NoCase("end"),                             Token.End),
    (NoCase("null"),                            Token.Null),
    (NoCase("accept"),                          Token.Accept),
    (NoCase("entry"),                           Token.Entry),
    (NoCase("select"),                          Token.Select),
    (NoCase("access"),                          Token.Access),
    (NoCase("exception"),                       Token.Exception),
    (NoCase("of"),                              Token.Of),
    (NoCase("separate"),                        Token.Separate),
    (NoCase("aliased"),                         Token.Aliased),
    (NoCase("exit"),                            Token.Exit),
    (NoCase("or"),                              Token.Or),
    (NoCase("some"),                            Token.Some),
    (NoCase("all"),                             Token.All),
    (NoCase("others"),                          Token.Others),
    (NoCase("subtype"),                         Token.Subtype),
    (NoCase("and"),                             Token.And),
    (NoCase("for"),                             Token.For),
    (NoCase("out"),                             Token.Out),
    (NoCase("synchronized"),                    Token.Synchronized),
    (NoCase("array"),                           Token.Array),
    (NoCase("function"),                        Token.Function),
    (NoCase("overriding"),                      Token.Overriding),
    (NoCase("at"),                              Token.At),
    (NoCase("tagged"),                          Token.Tagged),
    (NoCase("generic"),                         Token.Generic),
    (NoCase("package"),                         Token.Package),
    (NoCase("task"),                            Token.Task),
    (NoCase("begin"),                           Token.Begin),
    (NoCase("goto"),                            Token.Goto),
    (NoCase("pragma"),                          Token.Pragma),
    (NoCase("terminate"),                       Token.Terminate),
    (NoCase("body"),                            Token.Body),
    (NoCase("private"),                         Token.Private),
    (NoCase("then"),                            Token.Then),
    (NoCase("if"),                              Token.If),
    (NoCase("procedure"),                       Token.Procedure),
    (NoCase("type"),                            Token.Type),
    (NoCase("case"),                            Token.Case),
    (NoCase("in"),                              Token.In),
    (NoCase("protected"),                       Token.Protected),
    (NoCase("constant"),                        Token.Constant),
    (NoCase("interface"),                       Token.Interface),
    (NoCase("is"),                              Token.Is),
    (NoCase("raise"),                           Token.Raise),
    (NoCase("use"),                             Token.Use),
    (NoCase("declare"),                         Token.Declare),
    (NoCase("range"),                           Token.Range),
    (NoCase("delay"),                           Token.Delay),
    (NoCase("until"),                           Token.Until),
    (NoCase("limited"),                         Token.Limited),
    (NoCase("record"),                          Token.Record),
    (NoCase("when"),                            Token.When),
    (NoCase("delta"),                           Token.Delta),
    (NoCase("loop"),                            Token.Loop),
    (NoCase("rem"),                             Token.Rem),
    (NoCase("while"),                           Token.While),
    (NoCase("digits"),                          Token.Digits),
    (NoCase("renames"),                         Token.Renames),
    (NoCase("with"),                            Token.With),
    (NoCase("do"),                              Token.Do),
    (NoCase("mod"),                             Token.Mod),
    (NoCase("requeue"),                         Token.Requeue),
    (NoCase("xor"),                             Token.Xor),

    (Literal("("),                              Token.Par_open),
    (Literal(")"),                              Token.Par_close),
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

    (ada_lexer.patterns.decimal_literal,        Token.Number),

    (ada_lexer.patterns.based_literal,          Token.Number),

    (ada_lexer.patterns.identifier,             Token.Identifier),

    (Pattern(r"<<{ws}({identifier})?{ws}>>"),   Token.Label),

    (ada_lexer.patterns.p_string,               Token.String),
    (ada_lexer.patterns.p_percent_string,       Token.String),

    Case(Pattern("'.'"),
         Alt(prev_token_cond=(Token.Identifier, Token.All),
             send=Token.Tick,
             match_size=1),
         Alt(send=Token.Char, match_size=3)),
)
