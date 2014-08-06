import re
import os

fpath = os.path.abspath(os.path.dirname(__file__))


def get_file(name):
    return os.path.join(fpath, name)


class TokenMap(object):

    def __init__(self):
        qx_grammar = open(get_file("../../../cpp/src/ada.qx")).read()
        # noinspection PyTypeChecker
        els = map(lambda x: [y for y in x if y],
               re.findall(r'(?:\\C\{(.*?)\}|"(.*?)") => QUEX_TKN_(\w*)',
                          qx_grammar))
        print els
        self.str_to_names = dict(els)
        token_ids = open(get_file(
            "../../../cpp/src/EasyLexer-token_ids.h")).read()
        self.names_to_ids = {
            k: int(v)
            for k, v in re.findall(r'#define QUEX_TKN_(\w+).*?(\d+)', token_ids)
        }
        self.ids_to_names = {v: k.lower() for k, v in self.names_to_ids.items()}

token_map = TokenMap()
