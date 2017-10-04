with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Analysis.Implementation;
use Libadalang.Analysis.Implementation;

package Libadalang.Unit_Files is

   function Fetch_Unit
     (Ctx  : Analysis_Context;
      Name : Bare_Ada_Node;
      Kind : Unit_Kind) return Analysis_Unit;
   --  Fetch the unit for the file that (Name, Kind) designate. Populate
   --  populate its lexical environment and reference the result from Name's
   --  unit.
   --
   --  When Name is an illegal unit name (a call expression, for instance),
   --  this raises a Property_Error.

end Libadalang.Unit_Files;
