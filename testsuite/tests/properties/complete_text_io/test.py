import libadalang as lal


testv1 = """
procedure Test is
begin
   Ada.Text_IO.
end Test;
"""

testv2 = """
with Ada.Text_IO;

procedure Test is
begin
   Ada.Text_IO.
end Test;
"""


def print_visible_elements(u):
    # Find the incomplete dotted name
    dn = next(e for e in u.root.findall(lal.DottedName) if not e.f_suffix)
    for e in dn.p_complete:
        if e.is_visible:
            print(f"  - {e.decl.p_defining_name.text}")


ctx = lal.AnalysisContext()

print()
print("First attempt, result should be empty:")
print()
u = ctx.get_from_buffer("test.adb", testv1)
print_visible_elements(u)

print()
print("Second attempt, result should contain all visible entities of"
      " Ada.Text_IO:")
print()
u = ctx.get_from_buffer("test.adb", testv2)
print_visible_elements(u)

print()
print("Third, result should be back to empty:")
print()
u = ctx.get_from_buffer("test.adb", testv1)
print_visible_elements(u)
