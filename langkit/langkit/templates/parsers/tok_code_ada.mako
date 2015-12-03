## vim: filetype=makoada

--  Start tok_code

## Get the current token
${res} := Get_Token (Parser.TDH.all, ${pos_name});

if ${res}.Id /= ${token_kind} then
    ## If the result is not the one we expect, set pos to error
    ${pos} := -1;

    ## Document this failure so we can have a diagnostic at the end of parsing
    if Parser.Last_Fail.Pos <= ${pos_name} then
        Parser.Last_Fail.Pos := ${pos_name};
        Parser.Last_Fail.Expected_Token_Id := ${token_kind};
        Parser.Last_Fail.Found_Token_Id := ${res}.Id;
    end if;
else
## We don't want to increment the position if we are matching the termination
## token (eg. the last token in the token stream).
% if token_kind == get_context().lexer.token_name('termination'):
    ${pos} := ${pos_name};
## Else increment the current position
% else:
    ${pos} := ${pos_name} + 1;
% endif
end if;

--  End tok_code
