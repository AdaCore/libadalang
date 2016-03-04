import libadalang

import source


with open('pack.ads') as f:
    src_file = f.read()
src_file_lines = src_file.splitlines()

ctx = libadalang.AnalysisContext()
unit = ctx.get_from_buffer('pack.ads', src_file)
assert not unit.diagnostics, 'Got diagnostics: {}'.format(unit.diagnostics)


def src_slice(node):
    return source.src_slice(src_file_lines, node.sloc_range)


last_subp = None
subp_spec = None

# Go through all variable declarations
for obj_decl in unit.root.finditer(libadalang.ObjectDecl):

    # These always have a call expression initializer: get the corresponding
    # subprogram specification (there is no overloading involved, so using the
    # name is fine).
    call_expr = obj_decl.f_default_expr
    if last_subp is None or not last_subp.p_matches(call_expr.f_name):
        subp_spec = unit.root.find(lambda n: (
            isinstance(n, libadalang.SubprogramSpec) and
            n.f_name.p_matches(call_expr.f_name)
        ))

        if last_subp is not None:
            print ''

        print '-- {}'.format(src_slice(subp_spec))
        last_subp = subp_spec.f_name

    # Then, show the result of the match for each parameter association in this
    # call.
    print '  {}:'.format(src_slice(obj_decl.f_ids))

    for param_assoc, param_match in zip(
        call_expr.f_suffix.f_params,
        subp_spec.p_match_param_list(call_expr.f_suffix)
    ):
        print '    {}: has_matched={}, is_formal_opt={}'.format(
            src_slice(param_assoc).rjust(8),
            param_match.f_has_matched,
            param_match.f_is_formal_opt
        )
