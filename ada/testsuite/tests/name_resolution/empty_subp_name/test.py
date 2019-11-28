from __future__ import absolute_import, division, print_function

import libadalang as lal


bla_1 = b"""procedure bla is
begin
   null;
end;
"""

bla_2 = b"""procedure  is
begin
   null;
end;
"""


ctx = lal.AnalysisContext()
unit = ctx.get_from_buffer('bla.adb', bla_1)
unit.populate_lexical_env()

# The following used to raise a Constraint_Error
unit = ctx.get_from_buffer('bla.adb', bla_2)

print("Done.")
