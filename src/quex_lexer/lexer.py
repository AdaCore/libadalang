import cffi
import re
import os

fpath = os.path.abspath(os.path.dirname(__file__))


def get_file(name):
    return os.path.join(fpath, name)

re_include = re.compile("#include.*?$", re.M)
ffi = cffi.FFI()
ffi.cdef(
    re_include.sub('', open(get_file("cffi_interface.h")).read()).strip()
)
C = ffi.dlopen(get_file("_lexer.so"))


class Token(object):

    def __init__(self, ctok):
        self._id = ctok._id
        self.text = ffi.string(ctok.text)
        self.num = ctok.number
        self.line = ctok.line_n
        self.col = ctok.column_n


def get_string(self):
    if self._id == 0:
        return ''
    s = ffi.string(self.text)
    return s or token_map.ids_to_names[self._id]


class Tokenizer(object):

    def __init__(self, string):
        self.lex = C.make_lexer_from_string(string, len(string))
        self.tokens_buffer = [None] * 1024
        self.tokens = []
        self.current_offset = 0

    def get_next_chunk(self):
        for _ in range(32000):
            self.tokens.append(Token(C.receive(self.lex)))

    def get(self, offset):
        if len(self.tokens) <= offset:
            self.get_next_chunk()
        return self.tokens[offset]


    def __get(self, offset):
        if offset < self.current_offset - 1024:
            raise Exception("FAIL")

        while offset >= self.current_offset:
            self.tokens_buffer[self.current_offset % 1024] = C.receive(
                self.lex)
            self.current_offset += 1

        return self.tokens_buffer[offset % 1024]
        # return C.get(self.lex, offset)


class TokenMap(object):

    def __init__(self):
        qx_grammar = open(get_file("ada.qx")).read()
        # noinspection PyTypeChecker
        self.str_to_names = dict(re.findall(r'"(.*?)" => QUEX_TKN_(\w*)',
                                            qx_grammar))
        token_ids = open(get_file("EasyLexer-token_ids.h")).read()
        self.names_to_ids = {
            k: int(v)
            for k, v in re.findall(r'#define QUEX_TKN_(\w+).*?(\d+)', token_ids)
        }
        self.ids_to_names = {v: k.lower() for k, v in self.names_to_ids.items()}

token_map = TokenMap()
