------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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

with Libadalang.Implementation;    use Libadalang.Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Iterators.Extensions is

   -----------------------------
   -- Defining_Names_Contains --
   -----------------------------

   function Decl_Defines (Name : Text_Type) return Ada_Node_Predicate is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set (Decl_Defines_Predicate'
           (Size    => Name'Length,
            Name    => Name,
            Context => No_Analysis_Context,
            Symbol  => null));
      end return;
   end Decl_Defines;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Decl_Defines_Predicate; N : Ada_Node) return Boolean
   is
      function Check_Base_Id (Id : Base_Id) return Boolean
      is (Single_Tok_Node_P_Sym (Unwrap_Node (Id)) = P.Symbol);
      --  Return whether the symbol for Id matches P.Name

   begin
      if N.Is_Null or else N.Kind not in Ada_Basic_Decl then
         return False;
      end if;

      --  Make sure we have a symbol that correspond to N's context
      declare
         Ctx : constant Analysis_Context := N.Unit.Context;
      begin
         if P.Context /= Ctx then
            P.Context := Ctx;
            P.Symbol := Lookup_Symbol (Unwrap_Context (Ctx), P.Name);
         end if;
      end;

      --  Look for at least one defining name that matches the expected name
      for Def_Name of N.As_Basic_Decl.P_Defining_Names loop
         declare
            N : constant Name := Def_Name.F_Name;
         begin
            case N.Kind is
               when Ada_Identifier | Ada_String_Literal =>
                  if Check_Base_Id (N.As_Base_Id) then
                     return True;
                  end if;

               when Ada_Dotted_Name =>
                  if Check_Base_Id (N.As_Dotted_Name.F_Suffix) then
                     return True;
                  end if;

               when others =>
                  null;
            end case;
         end;
      end loop;

      return False;
   end Evaluate;

   -------------
   -- Xref_Is --
   -------------

   function Xref_Is
     (Name               : Defining_Name;
      Imprecise_Fallback : Boolean := False) return Ada_Node_Predicate is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set (Xref_Predicate'
           (Name => Name, Imprecise_Fallback => Imprecise_Fallback));
      end return;
   end Xref_Is;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Xref_Predicate; N : Ada_Node) return Boolean is
   begin
      return not N.Is_Null
             and then N.P_Gnat_Xref (P.Imprecise_Fallback) = P.Name;
   exception
      when Property_Error =>
         return False;
   end Evaluate;

end Libadalang.Iterators.Extensions;
