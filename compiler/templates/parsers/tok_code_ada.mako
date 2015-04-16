## vim: filetype=makoada

--  Start tok_code

## Get the current token
${res} := Get_Token (Parser.TDH.all, ${pos_name});

if ${res}.Id /= ${token_kind} then
    ## If the result is not the one we expect, set pos to error.
    ${pos} := -1;

    ## Document this failure so we can have a diagnostic at the end of parsing
    if Parser.Last_Fail.Pos <= ${pos_name} then
        Parser.Last_Fail.Pos := ${pos_name};
        Parser.Last_Fail.Expected_Token_Id := ${token_kind};
        Parser.Last_Fail.Found_Token_Id := ${res}.Id;
    end if;
else
    ## Else increment the current position
    ${pos} := ${pos_name} + 1;
end if;

--  End tok_code
