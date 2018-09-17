------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.   See the  GNU  --
-- General  Public  License for more details.   You should have received a  --
-- copy of the GNU General Public License  distributed with this software;  --
-- see  file  COPYING3.  If not, go to  http://www.gnu.org/licenses  for a  --
-- complete copy of the license.                                            --
------------------------------------------------------------------------------

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

package Libadalang.Unit_Files is

   package LAL renames Libadalang.Analysis;

   function Default_Provider return LAL.Unit_Provider_Interface'Class;
   --  Default implementation for the Unit_Provider mechanism. It assumes that
   --  each compilation unit gets its own source file in the current directory,
   --  named according to the GNAT convention: See
   --  <http://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn
   --  /the_gnat_compilation_model.html#file-naming-rules> for more details. It
   --  also assumes these sources use GNAT's default native runtime.

   function Unit_Text_Name (N : LAL.Name) return Text_Type;
   --  Turn the name of an unit represented as a Name node into a textual name.
   --  For instance: "Foo.Bar". Raise a Property_Error if a Property_Error if N
   --  is not a valid unit name.

   function Unit_String_Name (Name : Text_Type) return String;
   --  Assuming Name contains only characters in the following subset::
   --
   --     '.' | '-' | '_' | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z'
   --
   --  then turn it into a lower-case ASCII strings. Raise a Property_Error if
   --  this assumption is false.

   function Unit_String_Name (N : LAL.Name) return String is
     (Unit_String_Name (Unit_Text_Name (N)));

   function File_From_Unit
     (Name : String; Kind : Analysis_Unit_Kind) return String;
   --  Convert an unit name and unit kind into the default filename

   function Spec_File_Name (Name : String) return String is
     (File_From_Unit (Name, Unit_Specification));
   --  Convert an unit name string into the default filename we expect for its
   --  specification. For instance, this turns "Foo.Bar" into "foo-bar.ads".

   function Body_File_Name (Name : String) return String is
     (File_From_Unit (Name, Unit_Body));
   --  Convert an unit name string into the default filename we expect for its
   --  body. For instance, this turns "Foo.Bar" into "foo-bar.adb".

end Libadalang.Unit_Files;
