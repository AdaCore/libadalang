# TODO??? explain

import libadalang as lal

from source import src_slice


with open('foo.adb') as f:
    src_lines = f.readlines()

ctx = lal.AnalysisContext()
u = ctx.get_from_file('foo.adb')
u.populate_lexical_env()

i = u.root.find(lal.ObjectDecl)
t2 = i.f_type_expr.f_type_expr_variant.f_name

print('Symbol resolution for {}:'.format(src_slice(src_lines, t2.sloc_range)))
entities = t2.p_entities
for node in entities:
    print('<{} {}>'.format(
        type(node).__name__,
        ', '.join(n.f_tok.text for n in node.p_defining_names)
    ))
if not entities:
    print('<none>')

print 'Done.'
