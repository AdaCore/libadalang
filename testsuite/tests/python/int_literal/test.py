"""
Test that IntLiteral.p_denoted_value properly decodes all valid Ada integer
literals. Also exercise several erroneous cases that are still accepted by
Libadalang's lexer.
"""

import libadalang as lal


c = lal.AnalysisContext('utf-8')
u = c.get_from_file('foo.ads')

for decl in u.root.findall(lal.NumberDecl):
    name = decl.f_ids.text
    expr = decl.f_expr
    assert isinstance(expr, lal.IntLiteral)
    try:
        v = expr.p_denoted_value
    except lal.PropertyError:
        v = '<PropertyError>'
    print('{} ({}) -> {}'.format(name, expr.text, v))
