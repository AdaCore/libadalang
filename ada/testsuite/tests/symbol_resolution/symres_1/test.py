# Test the resolution of a identifier to the corresponding object declaration:
# pfx.p_entities[0] must return the Lol.Koin.Fu object declaration.

import libadalang as lal

src_buffer = """
package body Lol is
   package Koin is
      Fu : Integer;
   end Koin;

   procedure Bar is
      A, B : Integer;
   begin
      declare
         subtype Lol is Integer range 0 .. 10;
      begin
         A := B + 2 + Koin.Fu;
      end;
   end Bar;
end Lol;
"""

ctx = lal.AnalysisContext('utf-8')
unit = ctx.get_from_buffer('foo.adb', src_buffer)
unit.populate_lexical_env()

# The only prefix in our unit is Koin.Fu at line 13
pfx = unit.root.find(lal.Prefix)
pfx.p_entities[0].dump()

print 'Done.'
