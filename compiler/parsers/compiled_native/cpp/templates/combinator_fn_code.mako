## vim: filetype=cpp

Memo<${decl_type(_self.get_type())}> ${_self.gen_fn_name}_memo;

${fn_profile} {
    % for name, typ in defs:
    ${decl_type(typ)} ${name} ${" = " + typ.nullexpr() if typ.nullexpr() else ""};
    % endfor

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
        ${res} = m.instance;
        goto return_label;
    } else if (m.state == MemoState::Fail) {
#if DEBUG_MODE
        printf("%s", indent_str.c_str());
        printf("GOT FAIL MEMO FOR ${_self.gen_fn_name} at pos %ld\n", pos);
#endif
        current_pos = -1;
        goto return_label;
    }

    ${code}
    ${_self.gen_fn_name}_memo.set(pos, ${pos} != -1, ${res}, ${pos});
    % if _self.needs_refcount():
        % if _self.get_type().is_ptr:
        if (${res}) ${res}->inc_ref();
        % else:
            ${res}.inc_ref();
        % endif
    % elif is_row(_self):
        ${res}.inc_ref();
    % endif

#if DEBUG_MODE
    if (${pos} == -1) {
        printf("%s", indent_str.c_str());
        printf("SET FAIL MEMO FOR ${_self.gen_fn_name} at pos %ld\n", pos);
    } else {
        printf("%s", indent_str.c_str());
        printf("SET SUCCESS MEMO FOR ${_self.gen_fn_name} at pos %ld, restart_pos = %d\n", pos, ${pos});
    }
#endif

    current_pos = ${pos};

    return_label:

#if DEBUG_MODE
    indent_str.resize(indent_str.size() - 4);
    printf("%s", indent_str.c_str());
    printf("-- LEAVING ${_self.gen_fn_name}\n");
#endif

    return ${res};
}
