with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Analysis.Implementation;
use Libadalang.Analysis.Implementation;

package Libadalang.Unit_Files is

   type Symbol_Type_Array is array (Positive range <>) of Symbol_Type;

   function Fetch_Unit
     (Ctx            : Analysis_Context;
      Name           : Bare_Ada_Node;
      Kind           : Unit_Kind;
      Load_If_Needed : Boolean) return Analysis_Unit;

   function Fetch_Unit
     (Ctx            : Analysis_Context;
      Name           : Symbol_Type_Array;
      From_Unit      : Analysis_Unit;
      Kind           : Unit_Kind;
      Load_If_Needed : Boolean) return Analysis_Unit;
   --  Fetch the unit for the file that (Name, Kind) designate. Populate its
   --  lexical environment and reference the result from Name's unit.
   --
   --  When Name is an illegal unit name (a call expression, for instance),
   --  this raises a Property_Error.
   --
   --  If Load_If_Needed is true, the analysis unit is loaded when it's not
   --  already. Otherwise, it is not loaded in this case and this returns
   --  No_Analysis_Unit.

   procedure Fetch_Standard (Context : Analysis_Context);
   --  Create the "Standard" analysis unit in Context. This unit will be called
   --  "__standard".

end Libadalang.Unit_Files;
