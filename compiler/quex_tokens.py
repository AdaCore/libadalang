import re
import os

fpath = os.path.abspath(os.path.dirname(__file__))


def get_file(name):
    return os.path.join(fpath, name)


class TokenMap(object):

    def __init__(self):
        qx_grammar = open(get_file("../ada/ada.qx")).read()
        # noinspection PyTypeChecker
        els = map(lambda x: [y for y in x if y],
               re.findall(r'(?:\\C\{(.*?)\}|"(.*?)") => QUEX_TKN_(\w*)',
                          qx_grammar))
        self.str_to_names = dict(els)

token_map = TokenMap()
