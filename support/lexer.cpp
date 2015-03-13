#include <cstdlib>
#include <cstdint>

#include "lexer.hpp"

Token no_token = {0, NULL, SourceLocationRange()};

using namespace std;

void Lexer::populate_tokens() {
    this->buffer_tk._id = 1;
    while (this->buffer_tk._id != 0) {
        QUEX_NAME(token_p_set)(this->lexer, (quex_Token*)&this->buffer_tk);
        QUEX_NAME(receive)(this->lexer);
        this->symbolize();

        Token tk = {
            (uint16_t)this->buffer_tk._id,
            (const char *)this->buffer_tk.text,
            SourceLocationRange((uint32_t) this->buffer_tk._line_n,
                                (uint32_t) this->buffer_tk.end_line,
                                (uint16_t) this->buffer_tk._column_n,
                                (uint16_t) this->buffer_tk.end_column)
        };
        this->token_data_handler->tokens.push_back(tk);

        this->buffer_tk.last_id = this->token_data_handler->tokens.back().id;
    }
    QUEX_NAME(token_p_set)(this->lexer, (quex_Token*)&this->buffer_tk);
    QUEX_NAME(receive)(this->lexer);
    this->token_data_handler->tokens.push_back({
        (uint16_t)this->buffer_tk._id, NULL,
        SourceLocationRange((uint32_t) this->buffer_tk._line_n,
                            (uint32_t) this->buffer_tk.end_line,
                            (uint16_t) this->buffer_tk._column_n,
                            (uint16_t) this->buffer_tk.end_column)
    });
}

Lexer::Lexer (
    const char* filename,
    const char* char_encoding,
    TokenDataHandler* token_data_handler
) {
    this->lexer = new QUEX_TYPE_ANALYZER;
    this->buffer_ptr = nullptr;
    this->buffer_tk = {0, NULL, 0, 0, 0, 0, 0, 0, 0};
    this->current_offset = 0;
    this->max_pos = 0;
    this->max_token = no_token;
    this->token_data_handler = token_data_handler;

    QUEX_NAME(construct_file_name)(this->lexer, filename, char_encoding, false);
    QUEX_NAME(token_p_set)(this->lexer, &this->buffer_tk);
    this->populate_tokens();
}

Lexer::Lexer(
    const char* string,
    const size_t len,
    TokenDataHandler* token_data_handler
) {
    this->lexer = new QUEX_TYPE_ANALYZER;
    this->buffer_tk = {0, NULL, 0, 0, 0, 0, 0, 0, 0};
    this->current_offset = 0;
    this->max_pos = 0;
    this->max_token = no_token;
    this->token_data_handler = token_data_handler;

    char* buffer = (char*)malloc(len + 3);
    this->buffer_ptr = buffer;
    buffer++;

    strncpy(buffer + 1, string, len);
    buffer[0] = 0;
    buffer[len + 1] = 0;
    QUEX_NAME(construct_memory)(this->lexer, (uint8_t*)buffer,
                                    0, (uint8_t*)(buffer + len + 1), 0, false);

    QUEX_NAME(token_p_set)(this->lexer, &this->buffer_tk);
    this->populate_tokens();
}

Lexer::~Lexer() {
    this->max_token = no_token;
    QUEX_NAME (destruct) (this->lexer);
    delete this->lexer;
    if (this->buffer_ptr) free(this->buffer_ptr);
}

const char* empty_str="";

void Lexer::symbolize() {
    auto tk = &this->buffer_tk;

    if (tk->_id == QUEX_TKN_STRING) {
        tk->text = (uint8_t*) token_data_handler->add_string(
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
        (uint8_t*) token_data_handler->symbol_table->get((char*) tk->text, tk->len);
}
