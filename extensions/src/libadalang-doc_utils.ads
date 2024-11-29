--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with GNATCOLL.Strings_Impl; use GNATCOLL.Strings_Impl;

with Libadalang.Analysis; use Libadalang.Analysis;

--  This package provides basic capability to extract documentation associated
--  to declarations in sources files.
--
--  For now, it assumes that documentation is encoded in comments located near
--  the relevant source constructs, according to the GNAT coding style.
--
--  It also supports specially formatted comments called annotations,
--  interpreted as a key/value associations. Annotations are any comment line
--  of the form::
--
--     --% [annotation-name]: [annotation-value]
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

package Libadalang.Doc_Utils is

   use Support.Text;

   package XStrings is
   new GNATCOLL.Strings_Impl.Strings
      (SSize            => GNATCOLL.Strings_Impl.Optimal_String_Size,
       Character_Type   => Wide_Wide_Character,
       Character_String => Wide_Wide_String,
       To_Lower         => Support.Text.To_Lower);
   --  Wide_Wide XString type instantiation. TODO??? Would probably make sense
   --  to put that in Langkit_Support.

   package Annotations_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Text_Type, Text_Type, Ada.Strings.Wide_Wide_Hash, "=");

   type Doc_Type is record
      Doc : XStrings.XString;
      --  Documentation, where every line is concatenated as one XString

      Annotations : Annotations_Maps.Map;
      --  Annotations, as a key-value map of strings
   end record;
   --  Type representing the documentation of an entity

   function Get_Documentation (Decl : Basic_Decl) return Doc_Type;
   --  Return the documentation for given Basic_Decl.
   --
   --  Will raise a ``Property_Error`` if the doc is incorrectly formatted.

end Libadalang.Doc_Utils;
