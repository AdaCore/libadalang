Memo<${_self.get_type_string()}> ${_self.gen_fn_name}_memo;

${fn_profile} {
    % for name, decl in defs:
    ${decl.typestring} ${name} ${" = " + decl.default_val if decl.default_val else ""};
    % endfor

    auto m = ${_self.gen_fn_name}_memo.get(pos);

#if DEBUG_MODE
    printf(indent_str.c_str());
    printf("-- ENTERING ${_self.gen_fn_name} at pos %ld\n", pos);
    for (int i = 0; i < 4; i++) indent_str.append(" ");
#endif

    if (m.state == MemoState::Success) {
#if DEBUG_MODE
        printf(indent_str.c_str());
        printf("GOT SUCCESS MEMO FOR ${_self.gen_fn_name} at pos %d, restart pos = %d\n", pos, m.final_pos);
#endif
        current_pos = m.final_pos;
        ${res} = m.instance;
        goto return_label;
    } else if (m.state == MemoState::Fail) {
#if DEBUG_MODE
        printf(indent_str.c_str());
        printf("GOT FAIL MEMO FOR ${_self.gen_fn_name} at pos %d\n", pos);
#endif
        current_pos = -1;
        goto return_label;
    }

${code}
    ${_self.gen_fn_name}_memo.set(pos, ${pos} != -1, ${res}, ${pos});

#if DEBUG_MODE
    if (${pos} == -1) {
        printf(indent_str.c_str());
        printf("SET FAIL MEMO FOR ${_self.gen_fn_name} at pos %d\n", pos);
    } else {
        printf(indent_str.c_str());
        printf("SET SUCCESS MEMO FOR ${_self.gen_fn_name} at pos %d, restart_pos = %d\n", pos, ${pos});
    }
#endif

    current_pos = ${pos};

    return_label:

#if DEBUG_MODE
    indent_str.resize(indent_str.size() - 4);
    printf(indent_str.c_str());
    printf("-- LEAVING ${_self.gen_fn_name}\n");
#endif

    /* if (current_pos == -1) { // Need to free stuff
    % for name, decl in defs:
    % if decl.default_val == "nullptr":
    if (${name} != nullptr) free (${name});
    % endif
    % endfor
    } */

    return ${res};
}
