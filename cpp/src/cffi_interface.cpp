#include "cffi_interface.h"


#include <cstdint> /* Replace with <stdint.h> if appropriate */
#undef get16bits
#if (defined(__GNUC__) && defined(__i386__)) || defined(__WATCOMC__) \
  || defined(_MSC_VER) || defined (__BORLANDC__) || defined (__TURBOC__)
#define get16bits(d) (*((const uint16_t *) (d)))
#endif

#if !defined (get16bits)
#define get16bits(d) ((((uint32_t)(((const uint8_t *)(d))[1])) << 8)\
                       +(uint32_t)(((const uint8_t *)(d))[0]) )
#endif

Token no_token = {0, 0, 0, NULL};
quex_Token buffer_tk = {0, NULL, 0, 0, 0, 0};
Token max_token;
uint32_t last_id;
long max_pos;

uint32_t CharHash::operator() (const char * const string) const {

    const char* data = string;
    size_t len = strlen(data);
    uint32_t hash = (uint32_t)len;
    uint32_t tmp;
    int rem;

    if (len <= 0 || data == NULL) return 0;

    rem = len & 3;
    len >>= 2;

    /* Main loop */
    for (;len > 0; len--) {
        hash  += get16bits (data);
        tmp    = (get16bits (data+2) << 11) ^ hash;
        hash   = (hash << 16) ^ tmp;
        data  += 2*sizeof (uint16_t);
        hash  += hash >> 11;
    }

    /* Handle end cases */
    switch (rem) {
        case 3: hash += get16bits (data);
                hash ^= hash << 16;
                hash ^= (uint32_t)((signed char)data[sizeof (uint16_t)]) << 18;
                hash += hash >> 11;
                break;
        case 2: hash += get16bits (data);
                hash ^= hash << 11;
                hash += hash >> 17;
                break;
        case 1: hash += (unsigned char)*data;
                hash ^= hash << 10;
                hash += hash >> 1;
    }

    /* Force "avalanching" of final 127 bits */
    hash ^= hash << 3;
    hash += hash >> 5;
    hash ^= hash << 4;
    hash += hash >> 17;
    hash ^= hash << 25;
    hash += hash >> 6;

    return hash;
}

Lexer* make_lexer_from_file(const char* filename, const char* char_encoding) {
    Lexer* lex = new Lexer;
    lex->lexer = new quex_EasyLexer;
    lex->current_offset = 0;

    quex_EasyLexer_construct_file_name(lex->lexer, filename, char_encoding, false);
    quex_EasyLexer_token_p_set(lex->lexer, &buffer_tk);
    return lex;
}

Lexer* make_lexer_from_string(const char* string, const size_t len) {
    Lexer* lex = new Lexer;
    lex->lexer = new quex_EasyLexer;
    lex->current_offset = 0;

    char* buffer = (char*)malloc(len + 2);
    strncpy(buffer + 1, string, len);
    buffer[0] = 0;
    buffer[len + 1] = 0;
    quex_EasyLexer_construct_memory(lex->lexer, (uint8_t*)buffer, 
                                    0, (uint8_t*)(buffer + len + 1), 0, false);

    quex_EasyLexer_token_p_set(lex->lexer, &buffer_tk);
   return lex;
}

const char* empty_str="";

void symbolize(Lexer* lexer, quex_Token* tk) {

    static char text_buffer[1024];

    if (tk->_id == QUEX_TKN_STRING) {
        tk->text = (const uint8_t*)strndup((const char*)tk->text, tk->len);
        return;
    }

    if (!(tk->_id == QUEX_TKN_IDENTIFIER || tk->_id == QUEX_TKN_LABEL ||
          tk->_id == QUEX_TKN_CHAR ||
          tk->_id == QUEX_TKN_NUMBER ||
          tk->_id == QUEX_TKN_NULL)) {
        tk->text = (const uint8_t*)empty_str;
        tk->len = 0;
        return;
    }

    strncpy(text_buffer, (const char*)tk->text, tk->len);
    text_buffer[tk->len] = 0;

    auto it = lexer->hmap.find (text_buffer);
    if (it == lexer->hmap.end()) {
        tk->text = (const uint8_t*)strdup(text_buffer);
        lexer->hmap[(const char*)tk->text] = (const char*)tk->text;
    } else {
        tk->text = (const uint8_t*)it->second;
    }
}


Token receive(Lexer* lexer) {
    #pragma unused(lexer)
    return no_token;
    /*
    Token* tk = NULL;
    quex_EasyLexer_receive(lexer->lexer, (quex_Token**)&tk);
    Token owned_tk = *tk;
    symbolize(lexer, &owned_tk);
    return owned_tk;
    */
}
