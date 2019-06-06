from __future__ import absolute_import, division, print_function

import libadalang as lal

ctx = lal.AnalysisContext()


def test(strn):
    print(strn)
    print("=" * len(strn) + "\n")


test("Test that we can extract doc before the prelude")
u = ctx.get_from_buffer("test1", """
--  Documentation for the package
--  Bla bla bla

with A; use A;

package Foo is
end Foo;
""".strip())
print(u.root.find(lal.BasePackageDecl).p_doc)
print()

test("Test that we can extract doc after the prelude")
u = ctx.get_from_buffer("test1", """
with A; use A;

--  Documentation for the package
--  Bla bla bla

package Foo is
end Foo;
""".strip())
print(u.root.find(lal.BasePackageDecl).p_doc)
print()

test("Test annotation extraction")
u = ctx.get_from_buffer("test2", """
procedure Foo;
--% belongs-to: Bar
--%        random-annotation: True
--%other-annotation: False
--  This is the documentation for foo
""".strip())
print(u.root.find(lal.SubpDecl).p_doc_annotations)
print(u.root.find(lal.SubpDecl).p_doc)
print()

test("Test invalid documentation flagging")
u = ctx.get_from_buffer("test2", """
procedure Foo;
--% belongs-to: Bar
--%        random-annotation: True
--%other-annotation: False
--  This is the documentation for foo
-- Invalidly formatted
""".strip())
try:
    print(u.root.find(lal.SubpDecl).p_doc)
except lal.PropertyError, p:
    print(p.message)
    print()
