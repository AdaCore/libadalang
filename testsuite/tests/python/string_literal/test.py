"""
Test that StringLiteral.p_denoted_value properly decodes all valid Ada string
literals.
"""

import libadalang as lal


c = lal.AnalysisContext('utf-8')
u = c.get_from_file('foo.ads')

for decl in u.root.findall(lal.ObjectDecl):
    name = decl.f_ids.text
    expr = decl.f_default_expr
    assert isinstance(expr, lal.StringLiteral)
    try:
        v = expr.p_denoted_value
    except lal.PropertyError:
        v = u'<PropertyError>'
    print('{} ({}) -> {}'.format(name, ascii(expr.text), ascii(v)))
