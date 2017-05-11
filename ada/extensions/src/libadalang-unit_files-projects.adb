with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Unit_Files.Default;

package body Libadalang.Unit_Files.Projects is

   function Get_Unit
     (Provider    : Project_Unit_Provider_Type'Class;
      Context     : Analysis_Context;
      Name        : String;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False)
      return Analysis_Unit;
   --  Helper for Get_Unit primitives

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Provider    : Project_Unit_Provider_Type'Class;
      Context     : Analysis_Context;
      Name        : String;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit
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
            return Get_From_File (Context, +Full_Name (Path), Charset, Reparse,
                                  With_Trivia);
         end;
      end if;

      declare
         Dummy_File : constant String :=
            Libadalang.Unit_Files.Default.File_From_Unit (Name, Kind);
         Kind_Name  : constant String :=
           (case Kind is
            when Unit_Specification => "specification file",
            when Unit_Body          => "body file");
         Error      : constant String :=
            "Could not find source file for " & Name & " (" & Kind_Name & ")";
      begin
         return Get_With_Error (Context, Dummy_File, Error, Charset,
                                With_Trivia);
      end;
   end Get_Unit;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider_Type;
      Context     : Analysis_Context;
      Node        : Ada_Node;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit
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
         return Get_Unit (Provider, Context, Str_Name, Kind, Charset, Reparse,
                          With_Trivia);
      end;
   end Get_Unit;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider_Type;
      Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit
   is
      Str_Name : constant String :=
         Libadalang.Unit_Files.Default.Unit_String_Name (Name);
   begin
      return Get_Unit (Provider, Context, Str_Name, Kind, Charset, Reparse,
                       With_Trivia);
   end Get_Unit;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Provider : in out Project_Unit_Provider_Type)
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
     (Provider : in out Project_Unit_Provider_Type)
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
