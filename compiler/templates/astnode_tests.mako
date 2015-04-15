## vim: filetype=makocpp

% for test_name, (ada_string, res_ast_str) in zip(test_names, cls.tests):
void ${test_name} () {
    Parser parser = Parser(${c_repr(ada_string)}, ${len(ada_string)});
    auto res = parser.${trans_comb.gen_fn_name} (0);
    auto repr = res->repr();
    printf("%sResult for test ${test_name} : %s%s\n", blue, none, repr.c_str());

    if (repr == ${c_repr(res_ast_str)}) {
        printf("%sTest passed%s\n", green, none);
    } else {
        printf("%sTest failed!%s\n", red, none);
    }
}
% endfor
