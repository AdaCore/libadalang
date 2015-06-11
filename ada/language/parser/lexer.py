from langkit.lexer import (
    Lexer, NoCase, Literal, Pattern, Eof, NoText, Ignore, WithText,
    Case, Alt, Failure, Term
)

from langkit.enum import Enum


class Token(Enum):
    Identifier = 5000
    All = 5001

    # Keywords
    Abort = 5002
    Else = 5003
    New = 5004
    Return = 5005
    Abs = 5006
    Elsif = 5007
    Not = 5008
    Reverse = 5009
    Abstract = 5010
    End = 5011
    Null = 5012
    Accept = 5013
    Entry = 5014
    Select = 5015
    Access = 5016
    Exception = 5017
    Of = 5018
    Separate = 5019
    Aliased = 5020
    Exit = 5021
    Or = 5022
    Some = 5023
    Others = 5025
    Subtype = 5026
    And = 5027
    For = 5028
    Out = 5029
    Synchronized = 5030
    Array = 5031
    Function = 5032
    Overriding = 5033
    At = 5034
    Tagged = 5035
    Generic = 5036
    Package = 5037
    Task = 5038
    Begin = 5039
    Goto = 5040
    Pragma = 5041
    Terminate = 5042
    Body = 5043
    Private = 5044
    Then = 5045
    If = 5046
    Procedure = 5047
    Type = 5048
    Case = 5049
    In = 5050
    Protected = 5051
    Constant = 5052
    Interface = 5053
    Is = 5054
    Raise = 5055
    Use = 5056
    Declare = 5057
    Range = 5058
    Delay = 5059
    Until = 5060
    Limited = 5061
    Record = 5062
    When = 5063
    Delta = 5064
    Loop = 5065
    Rem = 5066
    While = 5067
    Digits = 5068
    Renames = 5069
    Withs = 5070
    Do = 5071
    Mod = 5072
    Requeue = 5073
    Xor = 5074

    # Label
    Label = 5075

    # Punctuation
    Par_close = 5076
    Par_open = 5077
    Semicolon = 5078
    Colon = 5079
    Comma = 5080
    Doubledot = 5081
    Dot = 5082
    Diamond = 5083
    Lte = 5084
    Gte = 5085
    Arrow = 5086
    Equal = 5087
    Lt = 5088
    Gt = 5089
    Plus = 5090
    Minus = 5091
    Power = 5092
    Mult = 5093
    Amp = 5094
    Notequal = 5095
    Divide = 5096
    Tick = 5097
    Pipe = 5098
    Assign = 5099

    # String and char literals
    String = 5100
    Char = 5101

    With = 5102
    Number = 5103

    # Fail token
    LexFail = 5104

ada_lexer = Lexer(Token)

ada_lexer.add_patterns(
    ('p_string', r"\"(\"\"|[^\n\"])*\""),
    ('digit', r"[0-9]"),
    ('extended_digit', r"[0-9a-zA-Z]"),
    ('integer', r"({digit}(_?{digit})*)"),
    ('exponent', r"([eE](\+?|-){integer})"),
    ('decimal_literal', r"{integer}(\.?{integer})?{exponent}?"),
    ('base', r"{integer}"),
    ('based_integer', r"{extended_digit}(_?{extended_digit})*"),
    ('based_literal',
     r"{base}#{based_integer}(\.{based_integer})?#{exponent}?"),
)

ada_lexer.add_rules(
    (Eof(),                                     NoText(Term)),
    (Pattern(r"[ \t\r\n]+"),                    Ignore()),
    (Pattern(r"--(.?)+"),                       Ignore()),
    (NoCase("abort"),                           NoText(Token.Abort)),
    (NoCase("else"),                            NoText(Token.Else)),
    (NoCase("new"),                             NoText(Token.New)),
    (NoCase("return"),                          NoText(Token.Return)),
    (NoCase("abs"),                             NoText(Token.Abs)),
    (NoCase("elsif"),                           NoText(Token.Elsif)),
    (NoCase("not"),                             NoText(Token.Not)),
    (NoCase("reverse"),                         NoText(Token.Reverse)),
    (NoCase("abstract"),                        NoText(Token.Abstract)),
    (NoCase("end"),                             NoText(Token.End)),
    (NoCase("null"),                            WithText(Token.Null)),
    (NoCase("accept"),                          NoText(Token.Accept)),
    (NoCase("entry"),                           NoText(Token.Entry)),
    (NoCase("select"),                          NoText(Token.Select)),
    (NoCase("access"),                          NoText(Token.Access)),
    (NoCase("exception"),                       NoText(Token.Exception)),
    (NoCase("of"),                              NoText(Token.Of)),
    (NoCase("separate"),                        NoText(Token.Separate)),
    (NoCase("aliased"),                         NoText(Token.Aliased)),
    (NoCase("exit"),                            NoText(Token.Exit)),
    (NoCase("or"),                              NoText(Token.Or)),
    (NoCase("some"),                            NoText(Token.Some)),
    (NoCase("all"),                             NoText(Token.All)),
    (NoCase("others"),                          NoText(Token.Others)),
    (NoCase("subtype"),                         NoText(Token.Subtype)),
    (NoCase("and"),                             NoText(Token.And)),
    (NoCase("for"),                             NoText(Token.For)),
    (NoCase("out"),                             NoText(Token.Out)),
    (NoCase("synchronized"),                    NoText(Token.Synchronized)),
    (NoCase("array"),                           NoText(Token.Array)),
    (NoCase("function"),                        NoText(Token.Function)),
    (NoCase("overriding"),                      NoText(Token.Overriding)),
    (NoCase("at"),                              NoText(Token.At)),
    (NoCase("tagged"),                          NoText(Token.Tagged)),
    (NoCase("generic"),                         NoText(Token.Generic)),
    (NoCase("package"),                         NoText(Token.Package)),
    (NoCase("task"),                            NoText(Token.Task)),
    (NoCase("begin"),                           NoText(Token.Begin)),
    (NoCase("goto"),                            NoText(Token.Goto)),
    (NoCase("pragma"),                          NoText(Token.Pragma)),
    (NoCase("terminate"),                       NoText(Token.Terminate)),
    (NoCase("body"),                            NoText(Token.Body)),
    (NoCase("private"),                         NoText(Token.Private)),
    (NoCase("then"),                            NoText(Token.Then)),
    (NoCase("if"),                              NoText(Token.If)),
    (NoCase("procedure"),                       NoText(Token.Procedure)),
    (NoCase("type"),                            NoText(Token.Type)),
    (NoCase("case"),                            NoText(Token.Case)),
    (NoCase("in"),                              NoText(Token.In)),
    (NoCase("protected"),                       NoText(Token.Protected)),
    (NoCase("constant"),                        NoText(Token.Constant)),
    (NoCase("interface"),                       NoText(Token.Interface)),
    (NoCase("is"),                              NoText(Token.Is)),
    (NoCase("raise"),                           NoText(Token.Raise)),
    (NoCase("use"),                             NoText(Token.Use)),
    (NoCase("declare"),                         NoText(Token.Declare)),
    (NoCase("range"),                           NoText(Token.Range)),
    (NoCase("delay"),                           NoText(Token.Delay)),
    (NoCase("until"),                           NoText(Token.Until)),
    (NoCase("limited"),                         NoText(Token.Limited)),
    (NoCase("record"),                          NoText(Token.Record)),
    (NoCase("when"),                            NoText(Token.When)),
    (NoCase("delta"),                           NoText(Token.Delta)),
    (NoCase("loop"),                            NoText(Token.Loop)),
    (NoCase("rem"),                             NoText(Token.Rem)),
    (NoCase("while"),                           NoText(Token.While)),
    (NoCase("digits"),                          NoText(Token.Digits)),
    (NoCase("renames"),                         NoText(Token.Renames)),
    (NoCase("with"),                            NoText(Token.With)),
    (NoCase("do"),                              NoText(Token.Do)),
    (NoCase("mod"),                             NoText(Token.Mod)),
    (NoCase("requeue"),                         NoText(Token.Requeue)),
    (NoCase("xor"),                             NoText(Token.Xor)),

    (Literal("("),                              NoText(Token.Par_open)),
    (Literal(")"),                              NoText(Token.Par_close)),
    (Literal(";"),                              NoText(Token.Semicolon)),
    (Literal(":"),                              NoText(Token.Colon)),
    (Literal(","),                              NoText(Token.Comma)),
    (Literal(".."),                             NoText(Token.Doubledot)),
    (Literal(":="),                             NoText(Token.Assign)),
    (Literal("."),                              NoText(Token.Dot)),
    (Literal("<>"),                             NoText(Token.Diamond)),
    (Literal("<="),                             NoText(Token.Lte)),
    (Literal(">="),                             NoText(Token.Gte)),
    (Literal("=>"),                             NoText(Token.Arrow)),
    (Literal("="),                              NoText(Token.Equal)),
    (Literal("<"),                              NoText(Token.Lt)),
    (Literal(">"),                              NoText(Token.Gt)),
    (Literal("+"),                              NoText(Token.Plus)),
    (Literal("-"),                              NoText(Token.Minus)),
    (Literal("**"),                             NoText(Token.Power)),
    (Literal("*"),                              NoText(Token.Mult)),
    (Literal("&"),                              NoText(Token.Amp)),
    (Literal("/="),                             NoText(Token.Notequal)),
    (Literal("/"),                              NoText(Token.Divide)),
    (Literal("'"),                              NoText(Token.Tick)),
    (Literal("|"),                              NoText(Token.Pipe)),

    (ada_lexer.patterns.decimal_literal,        WithText(Token.Number)),

    (ada_lexer.patterns.based_literal,          WithText(Token.Number)),

    (Pattern(r"[_a-zA-Z][_a-zA-Z0-9]*"),        WithText(Token.Identifier)),

    (Pattern(r"<<([_a-zA-Z][_a-zA-Z0-9]*)?>>"), WithText(Token.Label)),

    (ada_lexer.patterns.p_string,               WithText(Token.String)),

    Case(Pattern("'.'"),
         Alt(prev_token_cond=(Token.Identifier, Token.All),
             send=Token.Tick,
             match_size=1),
         Alt(send=Token.Char, match_size=3)),

    (Failure(),                NoText(Token.LexFail)),
)
