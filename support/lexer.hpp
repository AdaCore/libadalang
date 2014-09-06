#ifndef CFFI_INTERFACE_H
#define CFFI_INTERFACE_H

#include <assert.h>
#include <list>
#include <unordered_map>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <sstream>
#include "quex_lexer.h"

typedef struct _lexer Lexer;
extern uint32_t last_id;

enum RelativePosition {
    BEFORE,
    IN,
    AFTER
};

struct SourceLocation {
    uint32_t line;
    uint16_t column;

    SourceLocation() : line(0), column(0) {}
    SourceLocation(uint32_t line, uint16_t column)
      : line(line), column(column)
    {
        /* (0, 0) stands for the "null" value, but any other use of 0 is
           invalid otherwise.  */
        assert((line == 0) == (column == 0));
    }
    bool is_null() const { return line == 0; }

    /* Compute the relative position of SLOC with respect to this sloc.  */
    RelativePosition compare(const SourceLocation &sloc) const
    {
        assert (!is_null() && !sloc.is_null());
        if (sloc.line < line)
            return BEFORE;
        else if (line < sloc.line)
            return AFTER;
        /* In the following cases, both slocs are on the same line.  */
        else if (sloc.column < column)
            return BEFORE;
        else if (column < sloc.column)
            return AFTER;
        else
            return IN;
    }
    std::string repr() const {
        std::ostringstream oss;
        oss << line << ":" << column;
        return oss.str();
    }
};

struct SourceLocationRange {
    uint32_t start_line, end_line;
    uint16_t start_column, end_column;

    SourceLocationRange()
      : start_line(0),
        end_line(0),
        start_column(0),
        end_column(0)
    {}
    SourceLocationRange(SourceLocation start, SourceLocation end)
      : start_line(start.line),
        end_line(end.line),
        start_column(start.column),
        end_column(end.column)
    {
        assert(start.is_null() == end.is_null());
    }
    SourceLocationRange(uint32_t start_line, uint32_t end_line,
                        uint16_t start_column, uint16_t end_column)
      : start_line(start_line),
        end_line(end_line),
        start_column(start_column),
        end_column(end_column)
    {}

    SourceLocation get_start() const {
        return SourceLocation(start_line, start_column);
    }
    SourceLocation get_end() const {
        return SourceLocation(end_line, end_column);
    }
    bool is_null() const { return start_line == 0; }

    /* Compute the relative position of SLOC with respect to this sloc
       range.  */
    RelativePosition compare(const SourceLocation &sloc) const
    {
        assert(not sloc.is_null() && not is_null());
        switch (sloc.compare(get_start()))
        {
            case BEFORE:
                return BEFORE;
            case IN:
                return IN;
            case AFTER:
                if (sloc.compare(get_end()) == AFTER)
                    return AFTER;
                else
                    return IN;
        }
    }

    std::string repr() const {
        return get_start().repr() + "-" + get_end().repr();
    }
};

struct _token {
    uint16_t id;
    const char* text;
    SourceLocationRange sloc_range;
} __attribute__((packed));

typedef struct _token Token;

struct CharHash {
    uint32_t operator() (const char * const string) const;
};

struct eqstr {
    bool operator() (const char* s1, const char* s2) const {
        return strcmp(s1, s2) == 0;
    }
};

struct _lexer {
    QUEX_TYPE_ANALYZER* lexer;
    quex_Token tk_memory[1024];
    long current_offset;
    std::unordered_map<char*, char*, CharHash, eqstr> hmap;
    std::list<uint8_t*> str_literals;
    char* buffer_ptr;
};

extern Token no_token;
extern quex_Token buffer_tk;
extern Token max_token;
extern long max_pos;

Lexer* make_lexer_from_file(const char* filename, const char* char_encoding);
Lexer* make_lexer_from_string(const char* string, const size_t len);

void symbolize(Lexer* lexer, quex_Token* tk);

inline Token get(Lexer* lexer, long offset);

inline Token get(Lexer* lexer, long offset) {
    long coffset = lexer->current_offset;

#ifdef DEBUG_MODE
    if (offset < lexer->current_offset - 1024) {
        printf("Big failure in your pudding man\n");
        exit(1);
    }
#endif

    while (offset >= coffset) {
        QUEX_NAME(token_p_set)(lexer->lexer, (quex_Token*)&buffer_tk);
        QUEX_NAME(receive)(lexer->lexer);
        lexer->tk_memory[coffset % 1024] = buffer_tk;
        symbolize(lexer, &lexer->tk_memory[coffset % 1024]);
        last_id = lexer->tk_memory[coffset % 1024]._id;
        coffset++;
    }

    lexer->current_offset = coffset;
    auto qtk = lexer->tk_memory[offset % 1024];

    Token res = {
        (uint16_t)qtk._id,
        (const char *)qtk.text,
        SourceLocationRange((uint32_t) qtk._line_n, 0,
                            (uint16_t) qtk._column_n, 0)
    };

    if (offset > max_pos) {
        max_pos = offset;
        max_token = res;
    }

    return res;
}

void free_lexer (Lexer* lex);

#endif
