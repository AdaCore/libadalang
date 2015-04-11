## vim: filetype=makocpp

## Get the current token
${res} = this->lexer->get(${pos_name});

if (${res}.id != ${token_kind}) {
    ## If the result is not the one we expect, set pos to error.
    ${pos} = -1;

    ## Document this failure so we can have a diagnostic at the end of parsing
    if (last_fail.pos <= ${pos_name}) {
        last_fail.pos = ${pos_name};
        last_fail.expected_token_id = ${token_kind};
        last_fail.found_token_id = ${res}.id;
    }
} else {
    ## Else increment the current position
    ${pos} = ${pos_name} + 1;
}
