#ifndef LANGKIT_TEXT_H
#define LANGKIT_TEXT_H

#include <stdbool.h>
#include <stdio.h>

#include "libadalang.h"


/* Output on STREAM the text in TEXT, escaping non-ASCII characters with
   Python-style sequences. If WITH_QUOTES is false, do not output the boundary
   quotes.  */

static void
fprint_text(FILE *stream, ada_text text, bool with_quotes) {
    unsigned i;

    if (with_quotes)
        fputc('"', stream);

    for (i = 0; i < text.length; i++) {
        uint32_t c = text.chars[i];

        if ((with_quotes && c == '"')
            || c == '\\')
            fprintf(stream, "\\%c", (char) c);
        else if (0x20 <= c && c <= 0x7f) {
            fputc(c, stream);
        } else if (c <= 0xff) {
            fprintf(stream, "\\x");
            fprintf(stream, "%02x", c);
        } else if (c <= 0xffff) {
            fprintf(stream, "\\u");
            fprintf(stream, "%02x", c >> 8);
            fprintf(stream, "%02x", c & 0xff);
        } else {
            fprintf(stream, "\\U");
            fprintf(stream, "%02x", c >> 24);
            fprintf(stream, "%02x", (c >> 16) & 0xff);
            fprintf(stream, "%02x", (c >> 8) & 0xff);
            fprintf(stream, "%02x", c & 0xff);
        }
    }

    if (with_quotes)
        fputc('"', stream);
}

#endif /* LANGKIT_TEXT_H */
