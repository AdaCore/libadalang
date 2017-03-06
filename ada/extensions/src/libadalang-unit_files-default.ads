with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

--  This package provides an Unit_File_Provider implementation that is the
--  default one for Analysis_Context.

package Libadalang.Unit_Files.Default is

   type Default_Unit_File_Provider_Type is new Unit_File_Provider_Interface
      with private;
   --  Default implementation for the Unit_File_Provider mechanism. It assumes
   --  that all source files are in the current directory and that they follow
   --  the GNAT convention for file names.
   --  See <http://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn
   --       /the_gnat_compilation_model.html#file-naming-rules> for more
   --  details.

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Context  : Analysis_Context;
      Node     : Ada_Node;
      Kind     : Unit_Kind)
      return String;

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Context  : Analysis_Context;
      Name     : Text_Type;
      Kind     : Unit_Kind)
      return String;

   Default_Unit_File_Provider : constant Unit_File_Provider_Access_Cst;
   --  Singleton for Default_Unit_File_Provider_Type. Used as the default
   --  parameter for Libadalang.Analysis.Create.

   function Unit_Text_Name (N : Name) return Text_Type;
   --  Turn the name of an unit represented as a Name node into a textual name.
   --  For instance: "Foo.Bar". Raise a Property_Error if a Property_Error if N
   --  is not a valid unit name.

   function Unit_String_Name (Name : Text_Type) return String;
   --  Assuming Name contains only characters in the following subset::
   --
   --     '-' | '_' | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z'
   --
   --  then turn it into a lower-case ASCII string. Raise a Property_Error if
   --  this assumption is false.

   function Unit_String_Name (N : Name) return String is
     (Unit_String_Name (Unit_Text_Name (N)));

   function Spec_File_Name (Name : String) return String;
   --  Convert an unit name string into the default filename we expect for its
   --  specification. For instance, this turns "Foo.Bar" into "foo-bar.ads".

   function Body_File_Name (Name : String) return String;
   --  Convert an unit name string into the default filename we expect for its
   --  body. For instance, this turns "Foo.Bar" into "foo-bar.adb".

private

   type Default_Unit_File_Provider_Type is new Unit_File_Provider_Interface
      with null record;

   Default_Unit_File_Provider_Object : aliased Default_Unit_File_Provider_Type;
   Default_Unit_File_Provider : constant Unit_File_Provider_Access_Cst :=
      Default_Unit_File_Provider_Object'Access;

end Libadalang.Unit_Files.Default;
