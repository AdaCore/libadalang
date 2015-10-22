token_type {
    header {

    /* Redefine the stamp action to add the end line and end column to tokens */
    #undef QUEX_ACTION_TOKEN_STAMP
     #define QUEX_ACTION_TOKEN_STAMP(TOKEN_P)                \
        TOKEN_P->_line_n    = self_line_number_at_begin();    \
        TOKEN_P->_column_n  = self_column_number_at_begin();  \
        TOKEN_P->end_line   = self_line_number_at_end();      \
        TOKEN_P->end_column = self_column_number_at_end();

    }

    standard {
        id         : uint16_t;
    }

    distinct {
        text       : const QUEX_TYPE_CHARACTER*;
        len        : size_t;
        end_line   : size_t;
        end_column : uint16_t;
        last_id    : uint16_t;
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
