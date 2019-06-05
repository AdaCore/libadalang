------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with GNATCOLL.Strings_Impl; use GNATCOLL.Strings_Impl;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Doc_Utils is

   package XStrings is
   new GNATCOLL.Strings_Impl.Strings
      (SSize            => GNATCOLL.Strings_Impl.Optimal_String_Size,
       Character_Type   => Wide_Wide_Character,
       Character_String => Wide_Wide_String);

   package Annotations_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Text_Type, Text_Type, Ada.Strings.Wide_Wide_Hash, "=");

   type Doc_Type is record
      Doc : XStrings.XString;
      Annotations : Annotations_Maps.Map;
   end record;

   function Get_Documentation (Decl : Basic_Decl) return Doc_Type;
   --  Return the documentation for given Basic_Decl

end Libadalang.Doc_Utils;
