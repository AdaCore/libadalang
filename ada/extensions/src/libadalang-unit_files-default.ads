with Libadalang.AST; use Libadalang.AST;

package Libadalang.Unit_Files.Default is

   type Default_Unit_File_Provider_Type is new Unit_File_Provider_Interface
      with private;

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Node     : Ada_Node)
      return String;

   Default_Unit_File_Provider : constant Unit_File_Provider_Access_Cst;

private

   type Default_Unit_File_Provider_Type is new Unit_File_Provider_Interface
      with null record;

   Default_Unit_File_Provider_Object : aliased Default_Unit_File_Provider_Type;
   Default_Unit_File_Provider : constant Unit_File_Provider_Access_Cst :=
      Default_Unit_File_Provider_Object'Access;

end Libadalang.Unit_Files.Default;
