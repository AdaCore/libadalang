## vim: filetype=cpp

${start_sloc_range_var} = get(this->lexer, ${pos_name}).sloc_range;

% if _self.empty_valid:
    ${pos} = ${pos_name};
% else:
    ${pos} = -1;
% endif

% if _self.empty_valid:
    ${res} = new ASTList<${decl_type(_self.parser.get_type())}>;
% else:
    ${res} = ${_self.get_type().nullexpr()};
% endif

${cpos} = ${pos_name};

while (true) {
    ${parser_context.code}
    if (${parser_context.pos_var_name} == -1) break;

    ${pos} = ${parser_context.pos_var_name};
    % if cpos != parser_context.pos_var_name:
        ${cpos} = ${parser_context.pos_var_name};
    % endif

    % if _self.revtree_class:
        if (${res} == ${_self.get_type().nullexpr()})
            ${res} = ${parser_context.res_var_name};
        else {
            auto new_res = ${_self.revtree_class.name()}_new();
            new_res->${_self.revtree_class.fields[0].name} = ${res};
            new_res->${_self.revtree_class.fields[1].name} = ${parser_context.res_var_name};
            ${res}->inc_ref();
            ${parser_context.res_var_name}->inc_ref();
            ${res}->setParent(new_res);
            ${parser_context.res_var_name}->setParent(new_res);
            ${res} = new_res;
        }

    % else:
        if (${res} == ${_self.get_type().nullexpr()}) {
            ${res} = new ASTList<${decl_type(_self.parser.get_type())}>;
        }
        ${res}${"->" if _self.get_type().is_ptr else "."}vec.push_back
          (${parser_context.res_var_name});
        % if is_ast_node (_self.parser.get_type()):
             if (${parser_context.res_var_name}) ${parser_context.res_var_name}->setParent(${res});
        % endif

        % if _self.parser.needs_refcount():
            % if _self.parser.get_type().is_ptr:
                if (${parser_context.res_var_name})
                ${parser_context.res_var_name}->inc_ref();
            % else:
                ${parser_context.res_var_name}.inc_ref();
            % endif
        % endif

    % endif

    % if _self.sep:
        ${sep_context.code}
        if (${sep_context.pos_var_name} != -1) {
            ${cpos} = ${sep_context.pos_var_name};
        }
        else break;
    % endif
}

## If we managed to parse a list, compute and set the sloc range for this AST
## node.
if (${pos} != -1)
{
    ${res}${"->" if _self.get_type().is_ptr else "."}sloc_range_ =
        SourceLocationRange(${start_sloc_range_var}.get_start(),
                            (${cpos} == ${pos_name})
                            ? ${start_sloc_range_var}.get_end()
                            : get(this->lexer, ${cpos} - 1).sloc_range.get_end());
}
