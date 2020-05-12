import gc

import libadalang as lal


def show_resolve(pragma):
    e = pragma.f_args[0].f_expr
    ents = e.p_matching_nodes
    print('{} resolves to {}'.format(e, list(ents)))


new_foo = b"""
package Foo is
   type Integer is range 1 .. 10000;
   A : Integer  := 12;
   NN : Integer := 1249;
end Foo;
""".strip()

c = lal.AnalysisContext('utf-8')
u = c.get_from_file('foo-bar.ads')

for pragma in u.root.findall(lal.PragmaNode):
    show_resolve(pragma)
del pragma
gc.collect()

u2 = c.get_from_buffer('foo.ads', new_foo, reparse=True)

for pragma in u.root.findall(lal.PragmaNode):
    show_resolve(pragma)
