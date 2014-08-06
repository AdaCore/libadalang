#ifndef CFFI_INTERFACE_H
#define CFFI_INTERFACE_H

#include <list>
#include <unordered_map>
#include <algorithm> 
#include <functional> 
#include <cctype>
#include <locale>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include "EasyLexer.h"

typedef struct _lexer Lexer;
extern uint32_t last_id;

struct _token {
    uint16_t _id;
    uint16_t column_n;
    uint32_t line_n;
    const char* text;
} __attribute__((packed));

typedef struct _token Token;

struct eqstr {
    bool operator() (const char* s1, const char* s2) const {
        return strcmp(s1, s2) == 0;
    }
};


struct CharHash {
    uint32_t operator() (const char * const string) const;
};

struct _lexer {
    quex_EasyLexer* lexer;
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
        quex_EasyLexer_token_p_set(lexer->lexer, (quex_Token*)&buffer_tk);
        quex_EasyLexer_receive(lexer->lexer);
        lexer->tk_memory[coffset % 1024] = buffer_tk;
        symbolize(lexer, &lexer->tk_memory[coffset % 1024]);
        last_id = lexer->tk_memory[coffset % 1024]._id;
        coffset++;
    }

    lexer->current_offset = coffset;
    auto qtk = lexer->tk_memory[offset % 1024];

    Token res = {(uint16_t)qtk._id, (uint16_t)qtk._column_n, (uint32_t)qtk._line_n, (const char *)qtk.text};

    if (offset > max_pos) {
        max_pos = offset;
        max_token = res;
    }

    return res;
}

Token receive(Lexer* lexer);
void free_lexer (Lexer* lex);


enum class MemoState {
    Nores, Fail, Success
};

const int memo_size=256;

/*--------------------------
-- Memoization data types --
--------------------------*/
template <typename T> void dec_ref (T& el) { 
    el.dec_ref(); 
}
template <typename T> void dec_ref (T*& el) { 
    if (el) { 
        el->dec_ref(); 
        el = nullptr; 
    } 
}

template <typename T> struct Memo {

    struct MemoEntry {
        MemoState state;
        T instance;
        long offset, final_pos;
    };

    MemoEntry memo_array[memo_size];

    inline void clear() {
        for (int i = 0; i < memo_size; i++) {
            if (this->memo_array[i].state == MemoState::Success) {
#if DEBUG_MODE
                printf("dec ref at pos %d\n", i);
#endif
                dec_ref(this->memo_array[i].instance);
            }
            this->memo_array[i].state = MemoState::Nores;
        }
    }

    inline MemoEntry get(long offset) {
        auto res = this->memo_array[offset % memo_size];
        if (res.offset == offset) {
            return res;
        } else {
            MemoEntry ret;
            ret.state = MemoState::Nores;
            return ret;
        }
    }

    inline void set(long offset, bool success, T instance, long final_pos) {
        long off = offset % memo_size;
        // printf("IN MEMO SET, off = %d, offset = %d, state = %d\n", off, offset, memo_array[off].state);

        if (!(memo_array[off].state != MemoState::Nores && memo_array[off].offset == offset)) {
            
            // Do we need to free something ?
            if (memo_array[off].state == MemoState::Success) {
                dec_ref(memo_array[off].instance);
            }

            memo_array[off].final_pos = final_pos;
            memo_array[off].instance = instance;
            memo_array[off].offset = offset;
            memo_array[off].state = success ? MemoState::Success : MemoState::Fail;
        }
    }
};

// trim from start
static inline std::string &ltrim(std::string &s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
        return s;
}

// trim from end
static inline std::string &rtrim(std::string &s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
        return s;
}

// trim from both ends
static inline std::string &trim(std::string &s) {
        return ltrim(rtrim(s));
}

#endif
