from collections import defaultdict
from itertools import count
from template_utils import common_renderer
from common import TOKEN_PREFIX


class Matcher(object):
    """
    Base class for a matcher. A matcher specificies in which case a given
    input will trigger a match.
    """

    def render(self, lexer):
        """
        Render method to be overloaded in subclasses

        :param Lexer lexer: The instance of the lexer from which this render
          function has been called.
        :rtype: str
        """
        raise NotImplemented()


class Action(object):
    """
    Base class for an action. An action specificies what to do with a given
    match.
    """

    def render(self, lexer):
        """
        Render method to be overloaded in subclasses

        :param Lexer lexer: The instance of the lexer from which this render
          function has been called.
        :rtype: str
        """
        raise NotImplemented()


class TokenAction(Action):
    """
    Abstract Base class for an action that sends a token. Subclasses of
    TokenAction can *only* be used as the instantiation of a token kind, in the
    declaration of a LexerToken subclass, as in::

        class MyToken(LexerToken):
            Identifier = WithText()
            Keyword = NoText()
    """
    # This counter is used to preserve the order of TokenAction instantiations,
    # which allows us to get the declaration order of token enum kinds
    _counter = iter(count(0))

    def __init__(self):
        self._index = next(TokenAction._counter)
        self.name = ""
        self.lexer = None

    @property
    def value(self):
        return self._index


class NoText(TokenAction):
    """
    TokenAction. The associated token kind will never have any text associated
    to it::

        class MyToken(LexerToken):
            # Keyword tokens will never have text associated #with them
            Keyword = NoText()
    """

    def render(self, lexer):
        return "=> {};".format(lexer.token_name(self.name))


class WithText(TokenAction):
    """
    TokenAction. The associated token kind will have the lexed text associated
    to it. A new string will be allocated by the parser each time. Suited for
    literals (numbers, strings, etc..)::

        class MyToken(LexerToken):
            # String tokens will keep the associated text when lexed
            StringLiteral = WithText()
    """

    def render(self, lexer):
        return "=> {}(Lexeme);".format(lexer.token_name(self.name))


class WithTrivia(WithText):
    """
    TokenAction. The associated token kind will have the lexed text associated
    to it. A new string will be allocated by the parser each time. Suited for
    literals (numbers, strings, etc..)::

        class MyToken(LexerToken):
            # String tokens will keep the associated text when lexed
            StringLiteral = WithText()
    """
    pass


class WithSymbol(TokenAction):
    """
    TokenAction. When the associated token kind will be lexed, a token will be
    created with the text corresponding to the match, but as an internalized
    symbol, so that if you have two tokens with the same text, the text will be
    shared amongst both::

        class MyToken(LexerToken):
            # Identifiers will keep an internalized version of the text
            Identifier = WithSymbol()
    """

    def render(self, lexer):
        return "=> {}(Lexeme);".format(lexer.token_name(self.name))


class LexerTokenMetaclass(type):
    """
    Internal metaclass for LexerToken. Used to:
    - Associate names with corresponding token actions
    - Allow iteration on the LexerToken class to get back a list of token
      actions
    """
    def __new__(mcs, name, bases, dct):
        assert len(bases) == 1, (
            "Multiple inheritance for LexerToken subclasses is not supported"
        )

        fields = []
        for fld_name, fld_value in dct.items():
            if isinstance(fld_value, TokenAction):
                fld_value.name = fld_name
                fields.append(fld_value)

        dct['fields'] = getattr(bases[0], 'fields', []) + fields
        return type.__new__(mcs, name, bases, dct)

    def __iter__(cls):
        return (fld for fld in cls.fields)

    def __len__(cls):
        return len(cls.fields)


class LexerToken(object):
    """
    Base class from which your token class must derive. Every member needs to
    be an instanciation of a subclass of TokenAction, specifiying what is done
    with the resulting token.
    """
    __metaclass__ = LexerTokenMetaclass

    # Built-in termination token. Since it will always be the first token kind,
    # its value will always be zero
    Termination = NoText()

    # Built-in token to represent a lexing failure.
    LexingFailure = NoText()


class Patterns(object):
    """
    This is just a wrapper class, instantiated so that we can setattr patterns
    on it and the user can then type::

        mylexer.patterns.my_pattern

    To refer to a pattern.
    """
    pass


class Lexer(object):
    """
    This is the main lexer object, through which you will define your Lexer.
    At initialization time, you will need to provide an enum class to it, that
    will be used to identify the different kinds of tokens that your lexer can
    generate. This is a simple example for a simple calculator's lexer::

        from enum import Enum
        class TokenKind(Enum):
            Plus = 1
            Minus = 2
            Times = 3
            Div = 4
            Number = 5

        l = Lexer(TokenKind)

    You can add patterns to it, that are shortcuts to regex patterns, and that
    can refer to each others, like so::

        l.add_patterns(
            ('digit', r"[0-9]"),
            ('integer', r"({digit}(_?{digit})*)"),
        )

    Note that this is not necessary, just a convenient shortcut. After that
    you'll be able to define the match rules for your lexer, via the
    `add_rules` function::

        l.add_rules((
            (Literal("+"),       NoText(TokenKind.Plus))
            (Literal("-"),       NoText(TokenKind.Minus))
            (Literal("*"),       NoText(TokenKind.Times))
            (Literal("/"),       NoText(TokenKind.Div))
            (l.patterns.integer, WithText(TokenKind.Number))
        ))

    After that, your lexer is complete ! You can use it in your parser to
    generate parse trees.
    """

    class PredefPattern(Matcher):
        def __init__(self, name, pattern):
            self.name = name
            self.pattern = pattern

        def render(self, lexer):
            return "{{{}}}".format(self.name)

    def __init__(self, tokens_class):
        self.tokens_class = tokens_class
        assert issubclass(tokens_class, LexerToken)

        self.patterns = Patterns()
        self.__patterns = []
        self.rules = []
        self.tokens_set = {el.name.lower() for el in self.tokens_class}

        # This map will keep a mapping from literal matches to token kind
        # values, so that you can find back those values if you have the
        # literal that corresponds to it.
        self.literals_map = {}

        # TODO: Allow configuration of this prefix through __init__
        self.prefix = TOKEN_PREFIX

        # Map from token actions class names to set of token actions with that
        # class
        self.token_actions = defaultdict(set)

        for el in self.tokens_class:
            self.token_actions[type(el).__name__].add(el)

        # These are automatic rules, useful for all lexers: handle end of input
        # and invalid tokens.
        self.add_rules(
            (Eof(),     self.tokens_class.Termination),
            (Failure(), self.tokens_class.LexingFailure),
        )

    def add_patterns(self, *patterns):
        """
        Add the list of named patterns to the lexer's internal patterns. A
        named pattern is a pattern that you can refer to through the {}
        notation in another pattern, or directly via the lexer instance::

        l.add_patterns(
            ('digit', r"[0-9]"),
            ('integer', r"({digit}(_?{digit})*)"),
        )

        l.add_rules(
            (l.patterns.integer, WithText(TokenKind.Number))
            (Pattern("{integer}(\.{integer})?"), WithText(TokenKind.Number))
        )

        Please note that the order of addition matters if you want to refer to
        patterns in other patterns

        :param list[(str, str)] patterns: The list of patterns to add.
        """
        for k, v in patterns:
            predef_pattern = Lexer.PredefPattern(k, v)
            setattr(self.patterns, k.lower(), predef_pattern)
            self.__patterns.append(predef_pattern)

    def add_rules(self, *rules):
        """
        Add the list of rules to the lexer's internal list of rules. A rule is
        either:
          - A tuple of a Matcher and an Action to execute on this matcher. This
            is the common case
          - An instance of a class derived from `MatcherAssoc`. This is used to
            implement custom matching behaviour, such as in the case of `Case`.

        Please note that the order of addition matters. It will determine which
        rules are tried first by the lexer, so you could in effect make some
        rules 'dead' if you are not careful.

        :param rules: The list of rules to add.
        :type rules: list[(Matcher, Action)|RuleAssoc]
        """

        for matcher_assoc in rules:
            if type(matcher_assoc) is tuple:
                assert len(matcher_assoc) == 2
                matcher, action = matcher_assoc
                rule_assoc = RuleAssoc(matcher, action)
            else:
                assert isinstance(matcher_assoc, RuleAssoc)
                rule_assoc = matcher_assoc

            self.rules.append(rule_assoc)

            m, a = self.rules[-1].matcher, self.rules[-1].action
            if isinstance(m, Literal):
                # Add a mapping from the literal representation of the token to
                # itself, so that we can find tokens via their literal
                # representation.
                self.literals_map[m.to_match] = a

    def emit(self):
        """
        Return the content of the .qx file corresponding to this lexer
        specification. This function is not to be called by the client, and
        will be called by langkit when needed.

        :rtype: str
        """
        return common_renderer.render(
            "lexer/quex_lexer_spec",
            tokens_class=self.tokens_class,
            patterns=self.__patterns,
            rules=self.rules,
            lexer=self
        )

    def token_name(self, token):
        """
        Helper function to get a token's internal name from an instance of the
        token enum class, or from it's string representation (case
        insensitive).

        :param Enum|str token: The instance of the token enum class
        :rtype: str
        """
        if isinstance(token, TokenAction):
            name = token.name.upper()
        else:
            assert isinstance(token, str), (
                "Bad type for {}, supposed to be str|{}".format(
                    token, self.tokens_class
                )
            )
            if token.lower() in self.tokens_set:
                name = token.upper()
            elif token in self.literals_map:
                name = self.literals_map[token].name
            else:
                raise Exception(
                    "{} token literal is not part of the valid tokens for "
                    "this grammar".format(token)
                )

        return "{}{}".format(self.prefix, name.upper())


class Literal(Matcher):
    """
    Matcher. This matcher will match the string given in parameter,
    literally. This means that characters which would be special in a
    Pattern will be regular characters here::

        Pattern("a+")   # Matches one or more a
        Literal("a+")   # Matches "a" followed by "+"
    """
    def __init__(self, to_match):
        self.to_match = to_match

    def render(self, lexer):
        return '"{}"'.format(self.to_match)


class Pattern(Matcher):
    """
    Matcher. This will match a regular expression like pattern. Since the
    lexer DSL uses Quex underneath, you can find more documentation about
    the recognized regular expression language here:

       `Quex pattern language
       <http://quex.sourceforge.net/doc/html/usage/patterns/context-free.html>`_
    """

    def __init__(self, pattern):
        self.pattern = pattern

    def render(self, lexer):
        return self.pattern


class NoCase(Matcher):
    """
    Matcher. This is a shortcut for a case insensitive pattern, so that::

        Pattern(r"\C{abcd}")

    is equivalent to::

        NoCase("abcd")
    """

    def __init__(self, to_match):
        self.to_match = to_match

    def render(self, lexer):
        return '\C{{{}}}'.format(self.to_match)


class Eof(Matcher):
    """
    Matcher. Matches the end of the file/input stream.
    """
    def __init__(self):
        pass

    def render(self, lexer):
        return "<<EOF>>"


class Failure(Matcher):
    """
    Matcher. Matches a case of failure in the lexer.
    """
    def __init__(self):
        pass

    def render(self, lexer):
        return "on_failure"


class Ignore(Action):
    """
    Action. Basically ignore the matched text.
    """
    def render(self, lexer):
        return "{ }"


class RuleAssoc(object):
    """
    Base class for a matcher -> action association. This class should not be
    used directly, since you can provide a tuple to add_rules, that will be
    expanded to a RuleAssoc.
    """
    def __init__(self, matcher, action):
        self.matcher = matcher
        self.action = action

    def render(self, lexer):
        return "{} {}".format(
            self.matcher.render(lexer),
            self.action.render(lexer)
        )


class Alt(object):
    """
    Holder class used to specify the alternatives to a Case rule. Can only
    be used in this context.
    """
    def __init__(self, prev_token_cond=None, send=None, match_size=None):
        self.prev_token_cond = prev_token_cond
        self.send = send
        self.match_size = match_size


class Case(RuleAssoc):
    """
    Special rule association that enables dispatching the action depending
    on the previously parsed token. The canonical example is the one for
    which this class was added: In the Ada language, a tick character can be
    used either as the start of a character literal, or as an attribute
    expression.

    One way to disambiguate is by looking at the previous token. An
    attribute expression can only happen is the token to the left is an
    identifier or the "all" keyword. In the rest of the cases, a tick will
    correspond to a character literal, or be a lexing error.

    We can express that with the case rule this way::

        Case(Pattern("'.'"),
             Alt(prev_token_cond=(Token.Identifier, Token.All),
                 send=Token.Tick,
                 match_size=1),
             Alt(send=Token.Char, match_size=3)),

    If the previous token is an Identifier or an All, then we send
    Token.Tick, with a match size of 1. We need to specify that because if
    the lexer arrived here, it matched one tick, any char, and another tick,
    so it needs to rewind back to the first tick.

    Else, then we matched a regular character literal. We send it.
    """

    class CaseAction(Action):
        def __init__(self, max_match_len, *alts):
            super(Case.CaseAction, self).__init__()
            self.max_match_len = max_match_len
            assert all(isinstance(a, Alt) for a in alts), (
                "Invalid alternative to Case matcher"
            )
            assert alts[-1].prev_token_cond is None, (
                "The last alternative to a case matcher "
                "must have no prev token condition"
            )
            self.alts = alts[:-1]
            self.last_alt = alts[-1]

        def render(self, lexer):
            return common_renderer.render(
                "lexer/case_action",
                alts=self.alts,
                last_alt=self.last_alt,
                max_match_len=self.max_match_len,
                lexer=lexer
            )

    def __init__(self, matcher, *alts):
        # TODO: Add check on the matcher to verify that it is constant length,
        # eg. No +/* patterns. If possible. Might be hard to verify !
        super(Case, self).__init__(
            matcher, Case.CaseAction(len(matcher.pattern), *alts)
        )
