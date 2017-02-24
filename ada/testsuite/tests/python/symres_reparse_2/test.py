from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import libadalang as lal


def show_resolve(pragma):
    e = pragma.f_args[0].f_expr
    ents = e.p_entities
    print('{} resolves to {}'.format(e, list(ents)))


new_foo = """
package Foo is
   type Integer is range 1 .. 10000;
   A : Integer  := 12;
   NN : Integer := 1249;
end Foo;
""".strip()

c = lal.AnalysisContext('utf-8')
u = c.get_from_file('foo-bar.ads')
u.populate_lexical_env()

for pragma in u.root.findall(lal.PragmaNode):
    show_resolve(pragma)

u2 = c.get_from_buffer('foo.ads', new_foo, reparse=True)
u2.populate_lexical_env()

for pragma in u.root.findall(lal.PragmaNode):
    show_resolve(pragma)
