--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Extension to the generated Iterators API for Libadalang-specific entry
--  points.

with Langkit_Support.Symbols; use Langkit_Support.Symbols;

package Libadalang.Iterators.Extensions is

   function Decl_Defines (Name : Text_Type) return Ada_Node_Predicate;
   --  Implementation for Libadalang.Iterators.Decl_Defines

   function Xref_Is
     (Name               : Defining_Name;
      Imprecise_Fallback : Boolean := False) return Ada_Node_Predicate;
   --  Implementation for Libadalang.Iterators.Xref_Is

private

   type Decl_Defines_Predicate (Size : Natural) is
      new Ada_Node_Predicate_Interface with
   record
      Name : Text_Type (1 .. Size);

      Context : Analysis_Context;
      Symbol  : Symbol_Type;
      --  Cached symbol converted from Name
   end record;

   overriding function Evaluate
     (P : in out Decl_Defines_Predicate; N : Ada_Node) return Boolean;

   type Xref_Predicate is new Ada_Node_Predicate_Interface with record
      Name               : Defining_Name;
      Imprecise_Fallback : Boolean;
   end record;

   overriding function Evaluate
     (P : in out Xref_Predicate; N : Ada_Node) return Boolean;

end Libadalang.Iterators.Extensions;
