--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.Traces;

with Langkit_Support.Symbols;   use Langkit_Support.Symbols;

with Libadalang.Common;         use Libadalang.Common;
with Libadalang.Implementation; use Libadalang.Implementation;

private package Libadalang.Env_Hooks is

   Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LIBADALANG.ENV_HOOKS", Default => GNATCOLL.Traces.From_Config);

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
      Name               : Symbol_Type_Array;
      Kind               : Analysis_Unit_Kind;
      From_Unit          : Internal_Unit;
      Load_If_Needed     : Boolean;
      Do_Prepare_Nameres : Boolean := True;
      Not_Found_Is_Error : Boolean := False;
      Process_Parents    : Boolean := True) return Internal_Unit;
   --  Fetch the unit for the file that ``(Name, Kind)`` designate.
   --
   --  ``From_Unit`` must be the analysis unit whose analysis triggers the
   --  loading of ``(Name, Kind)``.
   --
   --  If ``Load_If_Needed`` is False and the unit to fetch is not loaded yet,
   --  do nothing and return null. Make sure it is loaded in all other cases.
   --
   --  If ``Do_Prepare_Nameres`` is True, populate the lexical environment of
   --  loaded analysis units and add a reference from ``From_Unit`` to the
   --  returned analysis unit.
   --
   --  ``Not_Found_Is_Error`` is forwarded to the ``Unit_Requested_Callback``
   --  even handler when that handler is called.
   --
   --  If ``Process_Parents`` is True, automatically load the "parents" for the
   --  unit to fetch (for instance: fetch ``A`` if the unit ``A.B`` is
   --  requested).

   procedure Fetch_Standard (Context : Internal_Context);
   --  Create the "Standard" analysis unit in Context. This unit will be called
   --  "__standard".

end Libadalang.Env_Hooks;
