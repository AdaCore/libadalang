#ifndef CFFI_INTERFACE_H
#define CFFI_INTERFACE_H

#include <list>
#include <unordered_map>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include "quex_lexer.h"

typedef struct _lexer Lexer;
extern uint32_t last_id;

struct _token {
    uint16_t _id;
    uint16_t column_n;
    uint32_t line_n;
    const char* text;
} __attribute__((packed));

typedef struct _token Token;

struct CharHash {
    uint32_t operator() (const char * const string) const;
};

struct eqstr {
    bool operator() (const char* s1, const char* s2) const {
        return strcmp(s1, s2) == 0;
    }
};

struct _lexer {
    QUEX_TYPE_ANALYZER* lexer;
    quex_Token tk_memory[1024];
    long current_offset;
    std::unordered_map<char*, char*, CharHash, eqstr> hmap;
    std::list<uint8_t*> str_literals;
    char* buffer_ptr;
};

extern Token no_token;
extern quex_Token buffer_tk;
extern Token max_token;
extern long max_pos;

Lexer* make_lexer_from_file(const char* filename, const char* char_encoding);
Lexer* make_lexer_from_string(const char* string, const size_t len);

void symbolize(Lexer* lexer, quex_Token* tk);

inline Token get(Lexer* lexer, long offset);

inline Token get(Lexer* lexer, long offset) {
    long coffset = lexer->current_offset;

#ifdef DEBUG_MODE
    if (offset < lexer->current_offset - 1024) {
        printf("Big failure in your pudding man\n");
        exit(1);
    }
#endif

    while (offset >= coffset) {
        QUEX_NAME(token_p_set)(lexer->lexer, (quex_Token*)&buffer_tk);
        QUEX_NAME(receive)(lexer->lexer);
        lexer->tk_memory[coffset % 1024] = buffer_tk;
        symbolize(lexer, &lexer->tk_memory[coffset % 1024]);
        last_id = lexer->tk_memory[coffset % 1024]._id;
        coffset++;
    }

    lexer->current_offset = coffset;
    auto qtk = lexer->tk_memory[offset % 1024];

    Token res = {(uint16_t)qtk._id, (uint16_t)qtk._column_n, (uint32_t)qtk._line_n, (const char *)qtk.text};

    if (offset > max_pos) {
        max_pos = offset;
        max_token = res;
    }

    return res;
}

void free_lexer (Lexer* lex);

#endif
