token_type {
   header {
       #include <stdio.h>
       #include <string.h>

       struct $TOKEN_CLASS_tag;

       extern const char*
       $TOKEN_CLASS_get_string(struct $TOKEN_CLASS_tag* me,  char*  buffer, size_t   BufferSize);

       extern const char*
       $TOKEN_CLASS_pretty_char_text(struct $TOKEN_CLASS_tag* me, char*   buffer, size_t  BufferSize);

#      if ! defined(__QUEX_OPTION_WCHAR_T_DISABLED)
       extern const wchar_t*
       $TOKEN_CLASS_pretty_wchar_text(struct $TOKEN_CLASS_tag* me, wchar_t*  buffer, size_t BufferSize);
#      endif

#undef QUEX_ACTION_TOKEN_STAMP
#define QUEX_ACTION_TOKEN_STAMP(TOKEN_P)                \
    TOKEN_P->_line_n    = self_line_number_at_begin();    \
    TOKEN_P->_column_n  = self_column_number_at_begin();  \
    TOKEN_P->end_line   = self_line_number_at_end();      \
    TOKEN_P->end_column = self_column_number_at_end();

$INCLUDE_CONVERTER_DECLARATION
   }
   standard {
        id            : uint32_t;
        line_number   : size_t;
        column_number : size_t;
   }
   distinct {
        text         : const QUEX_TYPE_CHARACTER*;
        number       : size_t;
        len          : size_t;
        end_line     : size_t;
        end_column   : size_t;
        last_id      : uint32_t;
   }

   inheritable;

   constructor {
       self.number = 0;
       self.text   = LexemeNull;
   }

   destructor {
       if( self.text != LexemeNull ) {
           self.text = LexemeNull;
       }
   }

   copy {
        self._id  = Other._id;

        if( self.text != LexemeNull ) {
            QUEX_NAME(MemoryManager_Text_free)((QUEX_TYPE_CHARACTER*)self.text);
        }
        if( Other.text != LexemeNull ) {
            self.text = QUEX_NAME(MemoryManager_Text_allocate)(
                                    sizeof(QUEX_TYPE_CHARACTER)
                                  * (QUEX_NAME(strlen)(Other.text) + 1));
            __QUEX_STD_memcpy((void*)self.text, (void*)Other.text,
                                sizeof(QUEX_TYPE_CHARACTER)
                              * (QUEX_NAME(strlen)(Other.text) + 1));
        }
        self.number = Other.number;
    #   ifdef     QUEX_OPTION_TOKEN_STAMPING_WITH_LINE_AND_COLUMN
        __QUEX_IF_COUNT_LINES(self._line_n        = Other._line_n);
        __QUEX_IF_COUNT_LINES(self.end_line       = Other.end_line);
        __QUEX_IF_COUNT_COLUMNS(self._column_n    = Other._column_n);
        __QUEX_IF_COUNT_COLUMNS(self.end_column   = Other.end_column);
    #   endif
   }

   take_text {

        if( Begin != LexemeNull ) {
            self.text = Begin;
            self.len = (size_t)(End - Begin);
        } else {
            self.text = LexemeNull;
            self.len = 0;
        }

        /* This token copied the text from the chunk into the string,
         * so we do not claim owneship over it.                       */
        return false;
   }

   body {
       /*
        */
   }

   repetition_set {
       self.number = N;
   }

   repetition_get {
       return self.number;
   }

   footer {
        const char*
        $TOKEN_CLASS_get_string($TOKEN_CLASS* me, char*   buffer, size_t  BufferSize)
        {
            const char*  token_type_str = $TOKEN_CLASS_map_id_to_name(me->_id);
            const char*  BufferEnd  = buffer + BufferSize;
            const char*  iterator   = 0;
            char*        writerator = 0;

            /* Token Type */
            iterator = token_type_str;
            writerator = buffer;
            while( (*iterator) && writerator != BufferEnd ) {
                *writerator++ = *iterator++;
            }

            /* Opening Quote */
            if( BufferEnd - writerator > 2 ) {
                *writerator++ = ' ';
                *writerator++ = '\'';
            }

            /* The String */
            $TOKEN_CLASS_pretty_char_text(me, writerator, (size_t)(BufferEnd - writerator));

            while( *writerator ) {
                ++writerator;
            }

            /* Closing Quote */
            if( BufferEnd - writerator > 1 ) {
                *writerator++ = '\'';
            }
            *writerator = '\0';
            return buffer;
        }

        const char*
        $TOKEN_CLASS_pretty_char_text($TOKEN_CLASS* me, char*   buffer, size_t  BufferSize)
        /* Provides a somehow pretty-print of the text in the token. Note, that the buffer
         * in case of UTF8 should be 4bytes longer than the longest expected string.       */
        {
            const QUEX_TYPE_CHARACTER*  source    = me->text;
            char*                       drain     = buffer;
            const char*                 DrainEnd  = buffer + BufferSize;

            const QUEX_TYPE_CHARACTER*  SourceEnd = me->text + (size_t)(QUEX_NAME(strlen)(source)) + 1;
            $CONVERTER_STRING(&source, SourceEnd, &drain, DrainEnd);
            return buffer;
        }

#       if ! defined(__QUEX_OPTION_WCHAR_T_DISABLED)
        const wchar_t*
        $TOKEN_CLASS_pretty_wchar_text($TOKEN_CLASS* me, wchar_t*  buffer, size_t    BufferSize)
        {
            wchar_t*                    drain     = buffer;
            const wchar_t*              DrainEnd  = buffer + (ptrdiff_t)BufferSize;
            const QUEX_TYPE_CHARACTER*  source    = me->text;
            const QUEX_TYPE_CHARACTER*  SourceEnd = me->text + (ptrdiff_t)(QUEX_NAME(strlen)(source)) + 1;

            $CONVERTER_WSTRING(&source, SourceEnd, &drain, DrainEnd);
            return buffer;
        }
#       endif

$INCLUDE_CONVERTER_IMPLEMENTATION
   }
}


header {
#include <stdlib.h>  /* for: atoi() */
}

token {
    % for tok in tokens_class:
    ${tok.name.upper()} = ${tok.value};
    % endfor
}

define {
    % for pattern in patterns:
    ${pattern.name}         ${pattern.pattern}
    % endfor
}

mode ONE_AND_ONLY {
    % for rule in rules:
    ${rule.render(lexer)}
    % endfor
}
