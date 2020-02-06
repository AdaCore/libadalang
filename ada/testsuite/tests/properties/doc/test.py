from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()


def test(label, buffer, pred=None):
    """
    Test a given p_doc scenario.

    :param str label: Description of the test.
    :param str buffer: Ada code on which to run the test.
    :param fn pred: Predicate function to choose the decl on which to run
        p_doc, in the source tree. Defaults to ``lambda n: True``.
    """

    print(label)
    print('=' * len(label))
    print()

    u = ctx.get_from_buffer('test.adb', buffer)
    if pred:
        decl = u.root.find(lambda n: pred(n) and n.is_a(lal.BasicDecl))
    else:
        decl = u.root.find(lal.BasicDecl)

    try:
        doc = decl.p_doc
        print('Doc:', lal._py2to3.text_repr(doc))
        print()
    except lal.PropertyError as exc:
        print('Exception:', *exc.args)
        print()
        return

    annotations = decl.p_doc_annotations
    if annotations:
        print('Annotations:')
        for a in annotations:
            print('  * {} = {}'.format(a.key, a.value))
        print()


test('Test that there is no crash when doc is missing', b"""
with A; use A;

package Foo is
end Foo;
""")


test('Test that we can extract doc before the prelude', b"""
--  Documentation for the package
--  Bla bla bla

with A; use A;

package Foo is
end Foo;
""")

test('Test that we can extract doc after the prelude', b"""
with A; use A;

--  Documentation for the package
--
--  Bla bla bla

package Foo is
end Foo;
""")

test('Test annotation extraction', b"""
procedure Foo;
--% belongs-to: Bar
--%        random-annotation: True
--%other-annotation: False
--  This is the documentation for foo
""")

test('Test double annotation', b"""
procedure Foo;
--% a: Foo
--% a: Bar
""")

test('Test whitespace stripping', b"""
procedure Foo;
--% belongs-to: Bar
--%        random-annotation: True
--%other-annotation: False
--  This is the documentation for foo
-- Weirdly formatted
""")

test('Test carriage return stripping', b"""\r
procedure Foo;\r
--  This is a regular comment.\r
--\r
--  Except that we have carriage returns.\r
""")

test('Test toplevel package without token before "package"',
     b"package Lol is end Lol;")


test('Test resilience to wrong annotation format', b"""
procedure Foo;
--% belongs-to
""")

test('Test generic package doc', b"""
package Foo is
    --  This is the documentation for package Bar
    generic
        A : Integer;
        --  Documentation for a formal.
    package Bar is
    end Bar;
end Foo;
 """, lambda n: n.is_a(lal.GenericPackageDecl))

test('Test internal generic package doc', b"""
package Foo is
    --  This is the documentation for package Bar
    generic
        A : Integer;
        --  Documentation for a formal.
    package Bar is
    end Bar;
end Foo;
 """, lambda n: n.is_a(lal.GenericPackageInternal))


print('test.py: done')
