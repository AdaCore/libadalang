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
    Lexer (
        const char* filename,
        const char* char_encoding,
        TokenDataHandler* token_data_handler
    );

    Lexer (
        const char* string,
        const size_t len,
        TokenDataHandler* token_data_handler
    );

    void symbolize();
    void populate_tokens();

    ~Lexer();

    QUEX_TYPE_ANALYZER* lexer;
    quex_Token tk_memory[1024];
    quex_Token buffer_tk;
    long current_offset;
    long max_pos;
    Token max_token;
    char* buffer_ptr;
    TokenDataHandler* token_data_handler;

    inline Token get(long offset) {
        auto ret = token_data_handler->tokens[offset]; 
        if (max_pos < offset) {
            max_pos = offset;
            max_token = ret;
        }

        return ret;
    }

};

extern Token no_token;

void free_lexer (Lexer* lex);

#endif
