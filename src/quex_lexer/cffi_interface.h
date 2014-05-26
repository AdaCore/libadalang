#include <stdlib.h>
#include <stdint.h>

typedef struct _lexer Lexer;

typedef struct _token {
    uint32_t _id;
    const char* text;
    size_t number;
    size_t line_n;
    size_t column_n;
} Token;

Lexer* make_lexer_from_file(const char* filename, const char* char_encoding);
Lexer* make_lexer_from_string(const char* string, const size_t len);
Token get(Lexer* lexer, long offset);
Token receive(Lexer* lexer);
