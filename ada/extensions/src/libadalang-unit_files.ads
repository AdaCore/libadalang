with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Analysis.Implementation;
use Libadalang.Analysis.Implementation;

package Libadalang.Unit_Files is

   type Symbol_Type_Array is array (Positive range <>) of Symbol_Type;

   function Fetch_Unit
     (Ctx  : Analysis_Context;
      Name : Bare_Ada_Node;
      Kind : Unit_Kind) return Analysis_Unit;

   function Fetch_Unit
     (Ctx       : Analysis_Context;
      Name      : Symbol_Type_Array;
      From_Unit : Analysis_Unit;
      Kind      : Unit_Kind) return Analysis_Unit;

   --  Fetch the unit for the file that (Name, Kind) designate. Populate its
   --  lexical environment and reference the result from Name's unit.
   --
   --
   --  When Name is an illegal unit name (a call expression, for instance),
   --  this raises a Property_Error.

   procedure Fetch_Standard (Context : Analysis_Context);
   --  Create the "Standard" analysis unit in Context. This unit will be called
   --  "standard.ads".

end Libadalang.Unit_Files;
