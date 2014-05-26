#include "cffi_interface.h"
#include "EasyLexer.h"

struct _lexer {
    quex_EasyLexer* lexer;
    Token tk_memory[1024];
    long current_offset;
};

Lexer* make_lexer_from_file(const char* filename, const char* char_encoding) {
    Lexer* lex = malloc(sizeof(Lexer));
    lex->lexer = malloc(sizeof(quex_EasyLexer));
    lex->current_offset = 0;
    quex_EasyLexer_construct_file_name(lex->lexer, filename, char_encoding, false);
    return lex;
}

Lexer* make_lexer_from_string(const char* string, const size_t len) {
    Lexer* lex = malloc(sizeof(Lexer));
    lex->lexer = malloc(sizeof(quex_EasyLexer));
    lex->current_offset = 0;
    char* buffer = malloc(len + 2);
    strncpy(buffer+1, string, len);
    buffer[0] = 0;
    buffer[len + 1] = 0;
    quex_EasyLexer_construct_memory(lex->lexer, (uint8_t*)buffer, 
                                    0, (uint8_t*)(buffer + len + 1), 0, false);
   return lex;
}

Token receive(Lexer* lexer) {
    Token* tk = NULL;
    quex_EasyLexer_receive(lexer->lexer, (quex_Token**)&tk);
    return *tk;
}

Token get(Lexer* lexer, long offset) {
    Token* tk = NULL;

    // printf("%li, %li\n", offset, lexer->current_offset);
    if (offset < lexer->current_offset - 1024) {
        printf("Big failure in your pudding man\n");
        exit(1);
    }

    while (offset >= lexer->current_offset) {
        quex_EasyLexer_receive(lexer->lexer, (quex_Token**)&tk);
        lexer->tk_memory[lexer->current_offset % 1024] = *tk;
        lexer->current_offset++;
    }

    return lexer->tk_memory[(offset) % 1024];
}
