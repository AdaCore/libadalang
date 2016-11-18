with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.AST;       use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;

package Libadalang.Unit_Files.Default is

   type Default_Unit_File_Provider_Type is new Unit_File_Provider_Interface
      with private;

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Node     : Ada_Node)
      return String;

   Default_Unit_File_Provider : constant Unit_File_Provider_Access_Cst;

   function Get_Unit_Name (N : Name) return Text_Type;
   --  Return an unit Name as a string. For instance: "Foo.Bar". Raise a
   --  Property_Error if N is not a valid unit name.

   function Get_Unit_Name (N : Name) return String;
   --  Return an unit Name as a string. For instance: "Foo.Bar". Raise a
   --  Property_Error if N is not a valid unit name or if it contains
   --  characters that are outside the following subset::
   --
   --     '-' | '_' | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z'

private

   type Default_Unit_File_Provider_Type is new Unit_File_Provider_Interface
      with null record;

   Default_Unit_File_Provider_Object : aliased Default_Unit_File_Provider_Type;
   Default_Unit_File_Provider : constant Unit_File_Provider_Access_Cst :=
      Default_Unit_File_Provider_Object'Access;

end Libadalang.Unit_Files.Default;
