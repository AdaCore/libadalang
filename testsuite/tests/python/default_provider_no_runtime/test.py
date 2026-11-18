"""
Check that the default provider works without a native Ada toolchain.
"""

import os

import libadalang as lal


os.environ["PATH"] = ""
ctx = lal.AnalysisContext()
unit = ctx.get_from_provider("foo", lal.AnalysisUnitKind.unit_specification)
assert not unit.diagnostics
assert isinstance(unit.root, lal.CompilationUnit)
print("Done.")
