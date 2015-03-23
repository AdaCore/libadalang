import re
import common


class TokenMap(object):

    def __init__(self, lexer_file):
        with open(lexer_file) as f:
            qx_grammar = f.read()

        # noinspection PyTypeChecker
        els = map(
            lambda x: [y for y in x if y],
            re.findall(
                r'(?:\\C\{{(.*?)\}}|"(.*?)") => {token_prefix}(\w*)'.format(
                    token_prefix=common.TOKEN_PREFIX),
                qx_grammar
            )
        )

        # This dict maps the token string representation, if they have one in
        # the quex syntax definition, to their internal names. For example, in
        # the ada definition:
        # "=" -> TOKEN_EQUAL
        self.str_to_names = dict(els)

        # This is the reverse mapping: It maps the internal name to the string
        # representation:
        # TOKEN_EQUAL -> "="
        self.names_to_str = {v: k for k, v in self.str_to_names.items()}

        token_defs_text = next(re.finditer(
            r'token \{(.*)?\}', qx_grammar, re.DOTALL
        )).group(1)

        # This dict maps the token internal names to their integer identifiers
        self.tokens = {
            a.group(1): int(a.group(2))
            for a in re.finditer(r'([\w_]+)\s*=\s*(\d+);', token_defs_text)
        }

        # This is the prefix that quex appends to every token kind, added here
        # for ease of use in the templates
        self.TOKEN_PREFIX = common.TOKEN_PREFIX

    def __repr__(self):
        return "<TokenMap {}>".format(self.str_to_names)


token_map = None


def init_token_map(compile_ctx):
    global token_map
    token_map = TokenMap(compile_ctx.lexer_file)
