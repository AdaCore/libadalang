## vim: filetype=makocpp

#include <stdlib.h>
#include <stdbool.h>

#include "quex_interface.h"
#include "quex_lexer.h"


struct Lexer {
      QUEX_TYPE_ANALYZER quex_lexer;
      void *text_buffer_begin;
      void *text_buffer_end;
      quex_Token buffer_tk;
};

static void
init_lexer(Lexer *lexer) {
    QUEX_NAME(token_p_set)(&lexer->quex_lexer, &lexer->buffer_tk);
    memset (&lexer->buffer_tk, 0, sizeof (lexer->buffer_tk));
}

static void
fill_buffer(Lexer *lexer) {
    lexer->text_buffer_begin
      = QUEX_NAME(buffer_fill_region_append_conversion_direct)(
        &lexer->quex_lexer,
        lexer->text_buffer_begin,
        lexer->text_buffer_end
    );
}

Lexer*
${capi.get_name("lexer_from_buffer")}(const char *string, const char *charset,
                                      size_t length) {
    Lexer* lexer = malloc(sizeof (Lexer));

    lexer->text_buffer_begin = (void *) string;
    lexer->text_buffer_end = (void*) string + length;

    /* 4 * the size of the input should be enough to hold the entire decoded
       input in Quex's internal buffer.  In theory it should be possible with
       Quex to have smaller buffers but we found bugs for lexing at buffer's
       boundary.  TODO: report this to Quex.  */
    QUEX_NAME(construct_memory)(&lexer->quex_lexer,
                                NULL, 4 * length, NULL, charset, false);
    init_lexer(lexer);
    fill_buffer(lexer);
    return lexer;
}

void
${capi.get_name("free_lexer")}(Lexer* lexer) {
    QUEX_NAME(destruct)(&lexer->quex_lexer);
    free(lexer);
}

int
${capi.get_name("next_token")}(Lexer* lexer, struct token* tok) {
    /* Some lexers need to keep track of the last token: give them this
       information.  */
    lexer->buffer_tk.last_id = lexer->buffer_tk._id;
    QUEX_NAME(receive)(&lexer->quex_lexer);

    tok->id = lexer->buffer_tk._id;
    tok->text = lexer->buffer_tk.text;
    tok->text_length = lexer->buffer_tk.len;
    tok->start_line = lexer->buffer_tk._line_n;
    tok->end_line = lexer->buffer_tk.end_line;
    tok->start_column = lexer->buffer_tk._column_n;
    tok->end_column = lexer->buffer_tk.end_column;

    return tok->id != 0;
}
