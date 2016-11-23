with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.AST.Types; use Libadalang.AST.Types;
with Libadalang.Unit_Files.Default;

package body Libadalang.Unit_Files.Projects is

   function Get_File
     (Provider : Project_Unit_File_Provider_Type'Class;
      Name     : String)
      return String;
   --  Helper for Get_File primitives

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Provider : Project_Unit_File_Provider_Type'Class;
      Name     : String)
      return String
   is
      File : constant Filesystem_String := File_From_Unit
        (Project   => Root_Project (Provider.Project.all),
         Unit_Name => Name,
         Part      => Unit_Spec,
         Language  => "Ada");
   begin
      if File'Length /= 0 then
         declare
            Path : constant GNATCOLL.VFS.Virtual_File :=
               GNATCOLL.Projects.Create (Provider.Project.all, File);
         begin
            return +Full_Name (Path);
         end;
      end if;

      raise Property_Error with "unit not found";
   end Get_File;

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Project_Unit_File_Provider_Type;
      Node     : Ada_Node)
      return String
   is
   begin
      if Node.all not in Name_Type'Class then
         raise Property_Error with "invalid AST node for unit name";
      end if;

      declare
         Name      : constant Libadalang.AST.Types.Name :=
            Libadalang.AST.Types.Name (Node);
         Str_Name  : constant String :=
            Libadalang.Unit_Files.Default.Unit_String_Name (Name);
      begin
         return Get_File (Provider, Str_Name);
      end;
   end Get_File;

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Project_Unit_File_Provider_Type;
      Name     : Text_Type)
      return String
   is
      Str_Name : constant String :=
         Libadalang.Unit_Files.Default.Unit_String_Name (Name);
   begin
      return Get_File (Provider, Str_Name);
   end Get_File;

end Libadalang.Unit_Files.Projects;
