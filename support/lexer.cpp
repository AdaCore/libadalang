#include <cstdlib>
#include <cstdint>

#include "lexer.hpp"

Token no_token = {0, NULL, SourceLocationRange()};

using namespace std;

void populate_tokens(Lexer* lexer) {
    lexer->buffer_tk._id = 1;
    while (lexer->buffer_tk._id != 0) {
        QUEX_NAME(token_p_set)(lexer->lexer, (quex_Token*)&lexer->buffer_tk);
        QUEX_NAME(receive)(lexer->lexer);
        symbolize(lexer, &lexer->buffer_tk);

        Token tk = {
            (uint16_t)lexer->buffer_tk._id,
            (const char *)lexer->buffer_tk.text,
            SourceLocationRange((uint32_t) lexer->buffer_tk._line_n,
                                (uint32_t) lexer->buffer_tk.end_line,
                                (uint16_t) lexer->buffer_tk._column_n,
                                (uint16_t) lexer->buffer_tk.end_column)
        };
        lexer->token_data_handler->tokens.push_back(tk);

        lexer->buffer_tk.last_id = lexer->token_data_handler->tokens.back().id;
    }
    QUEX_NAME(token_p_set)(lexer->lexer, (quex_Token*)&lexer->buffer_tk);
    QUEX_NAME(receive)(lexer->lexer);
    lexer->token_data_handler->tokens.push_back({
        (uint16_t)lexer->buffer_tk._id, NULL,
        SourceLocationRange((uint32_t) lexer->buffer_tk._line_n,
                            (uint32_t) lexer->buffer_tk.end_line,
                            (uint16_t) lexer->buffer_tk._column_n,
                            (uint16_t) lexer->buffer_tk.end_column)
    });
}

Lexer* make_lexer_from_file(
    const char* filename,
    const char* char_encoding,
    TokenDataHandler* token_data_handler
) {
    Lexer* lex = new Lexer;
    lex->lexer = new QUEX_TYPE_ANALYZER;
    lex->buffer_ptr = nullptr;
    lex->buffer_tk = {0, NULL, 0, 0, 0, 0, 0, 0, 0};
    lex->current_offset = 0;
    lex->max_pos = 0;
    lex->max_token = no_token;
    lex->token_data_handler = token_data_handler;

    QUEX_NAME(construct_file_name)(lex->lexer, filename, char_encoding, false);
    QUEX_NAME(token_p_set)(lex->lexer, &lex->buffer_tk);
    populate_tokens(lex);
    return lex;
}

Lexer* make_lexer_from_string(
    const char* string,
    const size_t len,
    TokenDataHandler* token_data_handler
) {
    Lexer* lex = new Lexer;
    lex->lexer = new QUEX_TYPE_ANALYZER;
    lex->buffer_tk = {0, NULL, 0, 0, 0, 0, 0, 0, 0};
    lex->current_offset = 0;
    lex->max_pos = 0;
    lex->max_token = no_token;
    lex->token_data_handler = token_data_handler;

    char* buffer = (char*)malloc(len + 3);
    lex->buffer_ptr = buffer;
    buffer++;

    strncpy(buffer + 1, string, len);
    buffer[0] = 0;
    buffer[len + 1] = 0;
    QUEX_NAME(construct_memory)(lex->lexer, (uint8_t*)buffer,
                                    0, (uint8_t*)(buffer + len + 1), 0, false);

    QUEX_NAME(token_p_set)(lex->lexer, &lex->buffer_tk);
    populate_tokens(lex);
    return lex;
}

void free_lexer (Lexer* lex) {
    lex->max_token = no_token;
    QUEX_NAME (destruct) (lex->lexer);
    delete lex->lexer;
    if (lex->buffer_ptr) free(lex->buffer_ptr);

    delete lex;
}

const char* empty_str="";

void symbolize(Lexer* lexer, quex_Token* tk) {

    if (tk->_id == QUEX_TKN_STRING) {
        tk->text = (uint8_t*) lexer->token_data_handler->add_string(
            (const char*)tk->text, tk->len
        );
        return;
    }

    if (!(tk->_id == QUEX_TKN_IDENTIFIER || tk->_id == QUEX_TKN_LABEL ||
          tk->_id == QUEX_TKN_CHAR || tk->_id == QUEX_TKN_NUMBER ||
          tk->_id == QUEX_TKN_NULL)) {
        tk->text = (const uint8_t*)empty_str;
        tk->len = 0;
        return;
    }

    tk->text =
        (uint8_t*) lexer->token_data_handler->symbol_table->get((char*) tk->text, tk->len);
}
