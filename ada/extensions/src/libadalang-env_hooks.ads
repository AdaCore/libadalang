with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Libadalang.Common;         use Libadalang.Common;
with Libadalang.Implementation; use Libadalang.Implementation;

private package Libadalang.Env_Hooks is

   type Symbol_Type_Array is array (Positive range <>) of Symbol_Type;

   function Fetch_Unit
     (Ctx            : Internal_Context;
      Name           : Bare_Name;
      Kind           : Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit;

   function Fetch_Unit
     (Ctx            : Internal_Context;
      Name           : Symbol_Type_Array;
      From_Unit      : Internal_Unit;
      Kind           : Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit;
   --  Fetch the unit for the file that (Name, Kind) designate. Populate its
   --  lexical environment and reference the result from Name's unit.
   --
   --  When Name is an illegal unit name (a call expression, for instance),
   --  this raises a Property_Error.
   --
   --  If Load_If_Needed is true, the analysis unit is loaded when it's not
   --  already. Otherwise, it is not loaded in this case and this returns
   --  No_Analysis_Unit.

   procedure Fetch_Standard (Context : Internal_Context);
   --  Create the "Standard" analysis unit in Context. This unit will be called
   --  "__standard".

   procedure Env_Hook (Unit : Internal_Unit; Node : Bare_Ada_Node);
   --  Callback for the lexical environment hook. Fetch the analysis units
   --  that are designated by Node. Node is assumed to be a list of names
   --  coming from a With_Decl node.

end Libadalang.Env_Hooks;
