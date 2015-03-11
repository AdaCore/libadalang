## vim: filetype=cpp

Memo<${decl_type(_self.get_type())}> ${_self.gen_fn_name}_memo;

${decl_type(_self.get_type())} Parser::${_self.gen_fn_name} (long pos) {

    % for name, typ in parser_context.var_defs:
    ${decl_type(typ)} ${name} ${" = " + typ.nullexpr() if typ.nullexpr() else ""};
    % endfor
    % if _self.is_left_recursive():
        long mem_pos = pos;
        ${decl_type(_self.get_type())} mem_res = ${_self.get_type().nullexpr()};
    % endif
    auto m = ${_self.gen_fn_name}_memo.get(pos);

#if DEBUG_MODE
    printf("%s", indent_str.c_str());
    printf("-- ENTERING ${_self.gen_fn_name} at pos %ld\n", pos);
    for (int i = 0; i < 4; i++) indent_str.append(" ");
#endif

    if (m.state == MemoState::Success) {
#if DEBUG_MODE
        printf("%s", indent_str.c_str());
        printf("GOT SUCCESS MEMO FOR ${_self.gen_fn_name} at pos %ld, restart pos = %ld\n", pos, m.final_pos);
#endif
        current_pos = m.final_pos;
        ${parser_context.res_var_name} = m.instance;
        goto return_label;
    } else if (m.state == MemoState::Fail) {
#if DEBUG_MODE
        printf("%s", indent_str.c_str());
        printf("GOT FAIL MEMO FOR ${_self.gen_fn_name} at pos %ld\n", pos);
#endif
        current_pos = -1;
        goto return_label;
    }

    % if _self.is_left_recursive():
        ${_self.gen_fn_name}_memo.set(pos, false, ${parser_context.res_var_name}, mem_pos);
    % endif

    % if _self.is_left_recursive():
        try_again:
    % endif

    /**************************
    *  MAIN COMBINATORS CODE  *
    **************************/

    ${parser_context.code}

    /******************************
    *  END MAIN COMBINATORS CODE  *
    ******************************/

    % if _self.is_left_recursive():
        if (${parser_context.pos_var_name} > mem_pos) {
            mem_pos = ${parser_context.pos_var_name};
            % if _self.needs_refcount():
                if (mem_res != nullptr) dec_ref(mem_res);
            % endif
            mem_res = ${parser_context.res_var_name};
            % if _self.needs_refcount():
                mem_res->inc_ref();
            % endif
            ${_self.gen_fn_name}_memo.set(pos, ${parser_context.pos_var_name} != -1, ${parser_context.res_var_name}, ${parser_context.pos_var_name});
            % if _self.needs_refcount():
                ${parser_context.res_var_name}->inc_ref();
            % endif
            goto try_again;
        } else if (mem_pos > pos) {
            % if _self.needs_refcount():
                if (${parser_context.res_var_name}) {
                    ${parser_context.res_var_name}->inc_ref();
                    ${parser_context.res_var_name}->dec_ref();
                }
            % endif

            % if _self.needs_refcount():
                mem_res->dec_ref();
            % endif
            ${parser_context.res_var_name} = mem_res;
            ${parser_context.pos_var_name} = mem_pos;
            goto no_memo;
        }
    % endif

    ${_self.gen_fn_name}_memo.set(pos, ${parser_context.pos_var_name} != -1, ${parser_context.res_var_name}, ${parser_context.pos_var_name});
    % if _self.needs_refcount():
        if (${parser_context.res_var_name}) ${parser_context.res_var_name}->inc_ref();
    % endif

    % if _self.is_left_recursive():
        no_memo:
    % endif

#if DEBUG_MODE
    if (${parser_context.pos_var_name} == -1) {
        printf("%s", indent_str.c_str());
        printf("SET FAIL MEMO FOR ${_self.gen_fn_name} at pos %ld\n", pos);
    } else {
        printf("%s", indent_str.c_str());
        printf("SET SUCCESS MEMO FOR ${_self.gen_fn_name} at pos %ld, restart_pos = %d\n", pos, ${parser_context.pos_var_name});
    }
#endif

    current_pos = ${parser_context.pos_var_name};

    return_label:

#if DEBUG_MODE
    indent_str.resize(indent_str.size() - 4);
    printf("%s", indent_str.c_str());
    printf("-- LEAVING ${_self.gen_fn_name}\n");
#endif

    return ${parser_context.res_var_name};
}
