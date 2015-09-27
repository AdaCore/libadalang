## vim: filetype=makocpp

#ifndef QUEX_INTERFACE_H
#define QUEX_INTERFACE_H

#include <stdint.h>


struct token {
    /* Kind for this token (identifier, keyword, ...). */
    uint16_t id;
    /* For token kinds that don't keep text, NULL.  Pointer to an
       UTF-32-encoded string otherwise (native endianity). */
    const uint32_t *text;
    /* Size (in 32-bit words) for text. */
    size_t text_length;
    /* Source location for this token.  */
    uint32_t start_line, end_line;
    uint16_t start_column, end_column;
};


typedef struct Lexer Lexer;

Lexer*
${capi.get_name("lexer_from_buffer")}(const char *string, size_t length);

void
${capi.get_name("free_lexer")}(Lexer* lexer);

int
${capi.get_name("next_token")}(Lexer* lexer, struct token* tok);

#endif
