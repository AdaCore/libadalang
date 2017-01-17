with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Unit_Files.Default;

package body Libadalang.Unit_Files.Projects is

   function Get_File
     (Provider : Project_Unit_File_Provider_Type'Class;
      Name     : String;
      Kind     : Unit_Kind)
      return String;
   --  Helper for Get_File primitives

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Provider : Project_Unit_File_Provider_Type'Class;
      Name     : String;
      Kind     : Unit_Kind)
      return String
   is
      File : constant Filesystem_String := File_From_Unit
        (Project   => Root_Project (Provider.Project.all),
         Unit_Name => Name,
         Part      => Convert (Kind),
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
      Node     : Ada_Node;
      Kind     : Unit_Kind)
      return String
   is
   begin
      if Node.all not in Name_Type'Class then
         raise Property_Error with "invalid AST node for unit name";
      end if;

      declare
         Name      : constant Libadalang.Analysis.Name :=
            Libadalang.Analysis.Name (Node);
         Str_Name  : constant String :=
            Libadalang.Unit_Files.Default.Unit_String_Name (Name);
      begin
         return Get_File (Provider, Str_Name, Kind);
      end;
   end Get_File;

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Project_Unit_File_Provider_Type;
      Name     : Text_Type;
      Kind     : Unit_Kind)
      return String
   is
      Str_Name : constant String :=
         Libadalang.Unit_Files.Default.Unit_String_Name (Name);
   begin
      return Get_File (Provider, Str_Name, Kind);
   end Get_File;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Provider : in out Project_Unit_File_Provider_Type)
   is
   begin
      Provider.Project := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Provider : in out Project_Unit_File_Provider_Type)
   is
   begin
      if Provider.Is_Project_Owner then
         Unload (Provider.Project.all);
         Free (Provider.Project);
         Free (Provider.Env);
      end if;
      Provider.Project := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Finalize;

end Libadalang.Unit_Files.Projects;
