------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
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
