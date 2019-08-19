"""
Test that StringLiteral.p_denoted_value properly decodes all valid Ada string
literals.
"""

from __future__ import absolute_import, division, print_function

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
        v = '<PropertyError>'
    print('{} ({}) -> {}'.format(name, repr(expr.text), repr(v)))
