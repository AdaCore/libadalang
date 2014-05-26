/* -*- C++ -*-   vim: set syntax=cpp: 
 * (C) 2004-2009 Frank-Rene Schaefer
 * ABSOLUTELY NO WARRANTY
 */
#ifndef __QUEX_INCLUDE_GUARD__TOKEN__GENERATED__QUEX___TOKEN
#define __QUEX_INCLUDE_GUARD__TOKEN__GENERATED__QUEX___TOKEN

/* For '--token-class-only' the following option may not come directly
 * from the configuration file.                                        */
#ifndef    __QUEX_OPTION_PLAIN_C
#   define __QUEX_OPTION_PLAIN_C
#endif
#include <quex/code_base/definitions>
#include <quex/code_base/asserts>
#include <quex/code_base/compatibility/stdint.h>

/* LexemeNull object may be used for 'take_text'. */
QUEX_NAMESPACE_LEXEME_NULL_OPEN
extern QUEX_TYPE_CHARACTER   QUEX_LEXEME_NULL_IN_ITS_NAMESPACE;
QUEX_NAMESPACE_LEXEME_NULL_CLOSE



#   line 2 "/home/amiard/apps/quex/quex/code_base/token/CDefault.qx"

       #include <stdio.h>
       #include <string.h>

       struct quex_Token_tag;

       extern const char* 
       quex_Token_get_string(struct quex_Token_tag* me,  char*  buffer, size_t   BufferSize); 

       extern const char* 
       quex_Token_pretty_char_text(struct quex_Token_tag* me, char*   buffer, size_t  BufferSize); 

#      if ! defined(__QUEX_OPTION_WCHAR_T_DISABLED)
       extern const wchar_t* 
       quex_Token_pretty_wchar_text(struct quex_Token_tag* me, wchar_t*  buffer, size_t BufferSize); 
#      endif

#include <quex/code_base/converter_helper/identity>
   

#   line 46 "EasyLexer-token.h"

 
typedef struct quex_Token_tag {
    QUEX_TYPE_TOKEN_ID    _id;

#   line 25 "/home/amiard/apps/quex/quex/code_base/token/CDefault.qx"
    const QUEX_TYPE_CHARACTER* text;
#   line 26 "/home/amiard/apps/quex/quex/code_base/token/CDefault.qx"
    size_t                     number;
#   line 56 "EasyLexer-token.h"


#   ifdef     QUEX_OPTION_TOKEN_STAMPING_WITH_LINE_AND_COLUMN
#       ifdef QUEX_OPTION_LINE_NUMBER_COUNTING
        QUEX_TYPE_TOKEN_LINE_N    _line_n;
#       endif
#       ifdef  QUEX_OPTION_COLUMN_NUMBER_COUNTING
        QUEX_TYPE_TOKEN_COLUMN_N  _column_n;
#       endif
#   endif

#   line 118 "/home/amiard/apps/quex/quex/code_base/token/CDefault.qx"

       /*
        */
   

#   line 74 "EasyLexer-token.h"

} quex_Token;

QUEX_INLINE void quex_Token_construct(quex_Token*);
QUEX_INLINE void quex_Token_copy_construct(quex_Token*, 
                                             const quex_Token*);
QUEX_INLINE void quex_Token_copy(quex_Token*, const quex_Token*);
QUEX_INLINE void quex_Token_destruct(quex_Token*);

/* NOTE: Setters and getters as in the C++ version of the token class are not
 *       necessary, since the members are accessed directly.                   */

QUEX_INLINE void 
quex_Token_set(quex_Token*            __this, 
                 const QUEX_TYPE_TOKEN_ID ID);

extern const char*  quex_Token_map_id_to_name(const QUEX_TYPE_TOKEN_ID);

QUEX_INLINE bool 
quex_Token_take_text(quex_Token*              __this, 
                       QUEX_TYPE_ANALYZER*        analyzer, 
                       const QUEX_TYPE_CHARACTER* Begin, const QUEX_TYPE_CHARACTER* End);

#ifdef QUEX_OPTION_TOKEN_REPETITION_SUPPORT
QUEX_INLINE size_t quex_Token_repetition_n_get(quex_Token*);
QUEX_INLINE void   quex_Token_repetition_n_set(quex_Token*, size_t);
#endif /* QUEX_OPTION_TOKEN_REPETITION_SUPPORT */



#endif /* __QUEX_INCLUDE_GUARD__TOKEN__GENERATED__QUEX___TOKEN */
