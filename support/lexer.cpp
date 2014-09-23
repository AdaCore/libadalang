#include "lexer.hpp"
#include <cstdlib>


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

Token no_token = {0, NULL, SourceLocationRange()};
quex_Token buffer_tk = {0, 0, NULL, 0, 0, 0, 0, 0};
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
};

Lexer* make_lexer_from_file(const char* filename, const char* char_encoding) {
    Lexer* lex = new Lexer;
    lex->lexer = new QUEX_TYPE_ANALYZER;
    lex->current_offset = 0;
    lex->buffer_ptr = nullptr;

    QUEX_NAME(construct_file_name)(lex->lexer, filename, char_encoding, false);
    QUEX_NAME(token_p_set)(lex->lexer, &buffer_tk);
    return lex;
}

Lexer* make_lexer_from_string(const char* string, const size_t len) {
    Lexer* lex = new Lexer;
    lex->lexer = new QUEX_TYPE_ANALYZER;
    lex->current_offset = 0;

    char* buffer = (char*)malloc(len + 3);
    lex->buffer_ptr = buffer;
    buffer++;

    strncpy(buffer + 1, string, len);
    buffer[0] = 0;
    buffer[len + 1] = 0;
    QUEX_NAME(construct_memory)(lex->lexer, (uint8_t*)buffer, 
                                    0, (uint8_t*)(buffer + len + 1), 0, false);

    QUEX_NAME(token_p_set)(lex->lexer, &buffer_tk);
   return lex;
}

void free_lexer (Lexer* lex) {
    max_token = no_token;
    for (auto kv : lex->hmap) free(kv.first);
    for (auto str : lex->str_literals) free(str);
    QUEX_NAME (destruct) (lex->lexer);
    delete lex->lexer;
    if (lex->buffer_ptr) free(lex->buffer_ptr);

    delete lex;
}

const char* empty_str="";

void symbolize(Lexer* lexer, quex_Token* tk) {

    static char text_buffer[1024];
    uint8_t *buffer = NULL;

    if (tk->_id == QUEX_TKN_STRING) {
        buffer = (uint8_t *)malloc (tk->len + 1);
        strncpy((char*)buffer, (const char*)tk->text, tk->len);
        buffer[tk->len] = 0;
        tk->text = buffer;
        lexer->str_literals.push_back((uint8_t*)tk->text);
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
        tk->text = (uint8_t*)strdup(text_buffer);
        lexer->hmap[(char*)tk->text] = (char*)tk->text;
    } else {
        tk->text = (uint8_t*)it->second;
    }
}
