"""
Check that Libadalang can resolve references to units stored in source files
with non-ascii filenames.
"""

import libadalang as lal


with open("prj.gpr", "w", encoding="utf-8") as f:
    f.write("""
project Prj is
   package Naming is
      for Spec ("Pkg") use "pkg_\xe9.ads";
   end Naming;
end Prj;
""")

with open("pkg_\xe9.ads", "w") as f:
    f.write("""
package Pkg is
   procedure P;
end Pkg;
""")

with open("test.adb", "w") as f:
    f.write("""
with Pkg;
procedure Test is
begin
   Pkg.P;
end Test;
""")


unit = lal.GPRProject("prj.gpr").create_context().get_from_file("test.adb")
for name in unit.root.findall(lal.Identifier):
    print(f"{name} resolves to: {name.p_referenced_decl()}")

print('Done.')
