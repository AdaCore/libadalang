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
        self.str_to_names = dict(els)

    def __repr__(self):
        return "<TokenMap {}>".format(self.str_to_names)


token_map = None


def init_token_map(compile_ctx):
    global token_map
    token_map = TokenMap(compile_ctx.lexer_file)
