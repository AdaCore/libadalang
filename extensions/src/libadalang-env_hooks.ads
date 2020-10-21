------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Langkit_Support.Symbols;   use Langkit_Support.Symbols;

with Libadalang.Common;         use Libadalang.Common;
with Libadalang.Implementation; use Libadalang.Implementation;

private package Libadalang.Env_Hooks is

   type Symbol_Type_Array is array (Positive range <>) of Symbol_Type;

   subtype Defining_Name_Nodes is Ada_Node_Kind_Type with Static_Predicate =>
      Defining_Name_Nodes in Ada_Base_Id | Ada_Dotted_Name | Ada_Defining_Name;
   --  Nodes that can appear inside a defining name. These are produced by the
   --  "simple_name" grammar rule, so by construction, it is not possible to
   --  get other nodes.

   function Name_To_Symbols (Name : Bare_Name) return Symbol_Type_Array
      with Pre => Name = null or else Name.Kind in Defining_Name_Nodes;
   --  Turn a simple name into the corresponding array of symbols

   function Fetch_Unit
     (Ctx                : Internal_Context;
      Name               : Bare_Name;
      Kind               : Analysis_Unit_Kind;
      Load_If_Needed     : Boolean;
      Do_Prepare_Nameres : Boolean := True) return Internal_Unit;

   function Fetch_Unit
     (Ctx                : Internal_Context;
      Name               : Symbol_Type_Array;
      From_Unit          : Internal_Unit;
      Kind               : Analysis_Unit_Kind;
      Load_If_Needed     : Boolean;
      Do_Prepare_Nameres : Boolean := True) return Internal_Unit;
   --  Fetch the unit for the file that (Name, Kind) designate. If
   --  Do_Prepare_Nameres is set, populate its lexical environment and
   --  reference the result from Name's unit.
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

end Libadalang.Env_Hooks;
