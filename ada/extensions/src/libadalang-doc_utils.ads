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
