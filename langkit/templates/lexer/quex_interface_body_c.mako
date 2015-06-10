## vim: filetype=makocpp

#include <stdlib.h>
#include <stdbool.h>

#include "quex_interface.h"
#include "quex_lexer.h"


struct Lexer {
  QUEX_TYPE_ANALYZER quex_lexer;
  char *text_buffer;
  quex_Token buffer_tk;
};

static void
init_lexer(Lexer *lexer) {
    QUEX_NAME(token_p_set)(&lexer->quex_lexer, &lexer->buffer_tk);
    memset (&lexer->buffer_tk, 0, sizeof (lexer->buffer_tk));
}

Lexer*
${capi.get_name("lexer_from_buffer")}(const char *string, size_t length) {
    Lexer* lexer = malloc(sizeof (Lexer));

    lexer->text_buffer = malloc(sizeof (char) * (length + 3));
    strncpy(lexer->text_buffer + 2, string, length);
    lexer->text_buffer[1] = 0;
    lexer->text_buffer[length + 2] = 0;
    memset (&lexer->buffer_tk, 0, sizeof (quex_Token));
    QUEX_NAME(construct_memory)(&lexer->quex_lexer,
                                (uint8_t*) (lexer->text_buffer + 1), 0,
                                (uint8_t*) (lexer->text_buffer + length + 2), 0,
                                false);
    init_lexer(lexer);
    return lexer;
}

void
${capi.get_name("free_lexer")}(Lexer* lexer) {
    QUEX_NAME(destruct)(&lexer->quex_lexer);
    if (lexer->text_buffer != NULL)
        free(lexer->text_buffer);
    free(lexer);
}

int
${capi.get_name("next_token")}(Lexer* lexer, struct token* tok) {
    /* Some lexers need to keep track of the last token: give them this
       information.  */
    lexer->buffer_tk.last_id = lexer->buffer_tk._id;
    QUEX_NAME(receive)(&lexer->quex_lexer);

    tok->id = lexer->buffer_tk._id;
    tok->text = (const char *) lexer->buffer_tk.text;
    tok->text_length = lexer->buffer_tk.len;
    tok->start_line = lexer->buffer_tk._line_n;
    tok->end_line = lexer->buffer_tk.end_line;
    tok->start_column = lexer->buffer_tk._column_n;
    tok->end_column = lexer->buffer_tk.end_column;

    return tok->id != 0;
}
