#ifndef LEXER_HPP
#define LEXER_HPP

#include "tokendatahandler.hpp"
#include "token.hpp"

#include <cassert>
#include <list>
#include <unordered_map>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <sstream>

extern "C" {
    #include "quex_lexer.h"
}


class Lexer {
public:
    QUEX_TYPE_ANALYZER* lexer;
    quex_Token tk_memory[1024];
    quex_Token buffer_tk;
    long current_offset;
    long max_pos;
    Token max_token;
    char* buffer_ptr;
    TokenDataHandler* token_data_handler;
};

extern Token no_token;

Lexer* make_lexer_from_file(
    const char* filename,
    const char* char_encoding,
    TokenDataHandler* token_data_handler
);

Lexer* make_lexer_from_string(
    const char* string,
    const size_t len,
    TokenDataHandler* token_data_handler
);

void symbolize(Lexer* lexer, quex_Token* tk);

inline Token get(Lexer* lexer, long offset);

inline Token get(Lexer* lexer, long offset) {
    return lexer->token_data_handler->tokens[offset];
}

void free_lexer (Lexer* lex);

#endif
