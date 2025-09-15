"""
Check that GPR2 loading options in the Python API correctly bind the GPR2
loading options in the Ada GPR2.Options API.

If the definition of the GPR2.Options.Option type changes:

* The C API should not break thanks to the hard code mapping between
  enumeration values and integer codes in Libadalang's sources (preserving
  stability for users of Libadalang).

* This test will fail, to warn Libadalang maintainers that the C bindings, and
  other bindings relying on it, need to be extended.
"""

import os.path
import sys

import libadalang as lal


gpr2 = lal.GPRProject("gpr2.gpr")
for f in gpr2.source_files(lal.SourceFilesMode.root_project):
    if os.path.basename(f) == "gpr2-options.ads":
        break
else:
    print("Could not find gpr2-options.ads")
    sys.exit(1)
u = lal.AnalysisContext().get_from_file(f)
option_type_decl = u.root.find(
    lambda n: isinstance(n, lal.ConcreteTypeDecl) and n.f_name.text == "Option"
)
print("Option values:")
for i, value in enumerate(option_type_decl.f_type_def.f_enum_literals):
    print(f"  {value.text} = {i}")
print()

print("Done")
