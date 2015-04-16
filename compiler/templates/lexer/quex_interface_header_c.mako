## vim: filetype=makocpp

#ifndef QUEX_INTERFACE_H
#define QUEX_INTERFACE_H

#include <stdint.h>


struct token {
    uint16_t id;
    const char* text;
    size_t text_length;
    uint32_t start_line, end_line;
    uint16_t start_column, end_column;
};


typedef struct Lexer Lexer;

Lexer*
${capi.get_name("lexer_from_filename")}(const char* filename,
                                        const char* char_encoding);

Lexer*
${capi.get_name("lexer_from_buffer")}(const char *string, size_t length);

void
${capi.get_name("free_lexer")}(Lexer* lexer);

int
${capi.get_name("next_token")}(Lexer* lexer, struct token* tok);

#endif
