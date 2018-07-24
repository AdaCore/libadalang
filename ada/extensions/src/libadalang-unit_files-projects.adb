with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Unit_Files.Default;

package body Libadalang.Unit_Files.Projects is

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider_Type;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String
   is
   begin
      GPR_Lock.Seize;
      declare

         Str_Name : constant String :=
           Libadalang.Unit_Files.Default.Unit_String_Name (Name);

         File : constant Filesystem_String := Prj.File_From_Unit
           (Project   => Prj.Root_Project (Provider.Project.all),
            Unit_Name => Str_Name,
            Part      => Convert (Kind),
            Language  => "Ada");
      begin

         if File'Length = 0 then
            GPR_Lock.Release;
            return "";
         end if;

         declare
            Path : constant GNATCOLL.VFS.Virtual_File :=
              Prj.Create (Provider.Project.all, File);
            Ret : String := +Full_Name (Path);
         begin
            GPR_Lock.Release;
            return Ret;
         end;
      end;

   exception
      when others =>
         GPR_Lock.Release;
         raise;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider_Type;
      Context     : LP.Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LP.Analysis_Unit
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return LP.Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Str_Name : constant String :=
               Libadalang.Unit_Files.Default.Unit_String_Name (Name);
            Dummy_File : constant String :=
               Libadalang.Unit_Files.Default.File_From_Unit (Str_Name, Kind);
            Kind_Name  : constant String :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant String :=
               "Could not find source file for " & Str_Name & " (" & Kind_Name
               & ")";
         begin
            return LP.Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Provider : in out Project_Unit_Provider_Type) is
   begin
      Provider.Project := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Provider : in out Project_Unit_Provider_Type) is
   begin
      GPR_Lock.Seize;

      if Provider.Is_Project_Owner then
         Prj.Unload (Provider.Project.all);
         Prj.Free (Provider.Project);
         Prj.Free (Provider.Env);
      end if;
      Provider.Project := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;

      GPR_Lock.Release;
   exception
      when others =>
         GPR_Lock.Release;
         raise;
   end Finalize;

end Libadalang.Unit_Files.Projects;
