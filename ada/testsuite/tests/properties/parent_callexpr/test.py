# Test the behavior of the p_parent_callexpr property on various children for
# CallExpr nodes.

import libadalang as lal

import source


with open('foo.adb') as f:
    src_lines = f.readlines()
src_slice = lambda node: source.src_slice(src_lines, node.sloc_range)


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('foo.adb')


calls = unit.root.find(lal.HandledStmts).f_stmts
simple_call, prefix_call, complex_prefix_call, nested_call = calls

for i, (call, name) in enumerate((
    (simple_call, simple_call.f_call.f_name),
    (simple_call, simple_call.f_call.f_suffix[0].f_expr),

    (prefix_call, prefix_call.f_call.f_name.f_prefix),
    (prefix_call, prefix_call.f_call.f_name.f_suffix),

    (complex_prefix_call, complex_prefix_call.f_call.f_name.f_prefix.f_prefix),
    (complex_prefix_call, complex_prefix_call.f_call.f_name.f_prefix.f_suffix),
    (complex_prefix_call, complex_prefix_call.f_call.f_name.f_suffix),

    (nested_call, nested_call.f_call.f_name.f_name),
    (nested_call, nested_call.f_call.f_name.f_suffix[0].f_expr),
)):
    if i > 0:
        print('')
    print(src_slice(call))
    column = name.sloc_range.start.column - call.sloc_range.start.column

    p = name.p_parent_callexpr
    p_text = (src_slice(p)
              if isinstance(p, lal.CallExpr) else
              str(p))
    print('{}^ parent_callexpr = {}'.format(
        ' ' * column, p_text
    ))
