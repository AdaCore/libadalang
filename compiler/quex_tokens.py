import re


class TokenMap(object):

    def __init__(self, lexer_file):
        with open(lexer_file) as f:
            qx_grammar = f.read()

        # noinspection PyTypeChecker
        els = map(lambda x: [y for y in x if y],
                  re.findall(r'(?:\\C\{(.*?)\}|"(.*?)") => QUEX_TKN_(\w*)',
                             qx_grammar))
        self.str_to_names = dict(els)


def init_token_map(compile_ctx):
    global token_map
    token_map = TokenMap(compile_ctx.lexer_file)
