from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()


def test(label, buffer):
    print(label)
    print('=' * len(label))
    print()

    u = ctx.get_from_buffer('test.adb', buffer)
    decl = u.root.find(lal.BasicDecl)
    try:
        print(decl.p_doc)
    except lal.PropertyError as exc:
        print(exc.message)
        print()
        return
    print()

    annotations = decl.p_doc_annotations
    if annotations:
        print('Annotations:')
        for a in annotations:
            print('  * {} = {}'.format(a.key, a.value))
        print()


test('Test that we can extract doc before the prelude', """
--  Documentation for the package
--  Bla bla bla

with A; use A;

package Foo is
end Foo;
""")

test('Test that we can extract doc after the prelude', """
with A; use A;

--  Documentation for the package
--
--  Bla bla bla

package Foo is
end Foo;
""")

test('Test annotation extraction', """
procedure Foo;
--% belongs-to: Bar
--%        random-annotation: True
--%other-annotation: False
--  This is the documentation for foo
""")

test('Test invalid documentation flagging', """
procedure Foo;
--% belongs-to: Bar
--%        random-annotation: True
--%other-annotation: False
--  This is the documentation for foo
-- Invalidly formatted
""")

print('test.py: done')
