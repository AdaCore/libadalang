with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.Regexp; use GNAT.Regexp;

with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Common;             use Libadalang.Common;
with Libadalang.Data_Decomposition; use Libadalang.Data_Decomposition;
with Libadalang.Iterators;          use Libadalang.Iterators;

procedure Main is

   function "+"
     (S : String) return Unbounded_String renames To_Unbounded_String;

   U : constant Analysis_Unit := Create_Context.Get_From_File ("pkg.ads");
   T : constant Base_Type_Decl :=
     Find_First (U.Root, Kind_Is (Ada_Concrete_Type_Decl)).As_Base_Type_Decl;

   procedure Check_Directories
     (Label : String; Name_Pattern : Regexp; Directories : Filename_Array);
   --  Run Load_From_Directories on the arguments and lookup T

   procedure Check_Directory
     (Label : String; Name_Pattern : Regexp; Directory : String);
   --  Run Load_From_Directory on the arguments and lookup T

   procedure Check (Repinfo : Repinfo_Collection);
   --  Lookup T in Repinfo

   -----------------------
   -- Check_Directories --
   -----------------------

   procedure Check_Directories
     (Label : String; Name_Pattern : Regexp; Directories : Filename_Array)
   is
      Repinfo : Repinfo_Collection;
   begin
      Put_Line ("== Dirs: " & Label & " ==");
      New_Line;

      begin
         Repinfo := Load_From_Directories (Name_Pattern, Directories);
      exception
         when Exc : Loading_Error =>
            Put_Line ("Loading_Error: " & Exception_Message (Exc));
            New_Line;
            return;
      end;

      Check (Repinfo);
   end Check_Directories;

   ---------------------
   -- Check_Directory --
   ---------------------

   procedure Check_Directory
     (Label : String; Name_Pattern : Regexp; Directory : String)
   is
      Repinfo : Repinfo_Collection;
   begin
      Put_Line ("== Dir: " & Label & " ==");
      New_Line;

      begin
         Repinfo := Load_From_Directory (Name_Pattern, Directory);
      exception
         when Exc : Loading_Error =>
            Put_Line ("Loading_Error: " & Exception_Message (Exc));
            New_Line;
            return;
      end;

      Check (Repinfo);
   end Check_Directory;

   -----------
   -- Check --
   -----------

   procedure Check (Repinfo : Repinfo_Collection) is
      T_Info : constant Type_Representation := Repinfo.Lookup (T);
   begin
      if Is_Null (T_Info) then
         Put_Line ("Cannot find type representation of " & T.Image);
         New_Line;
         return;
      end if;

      Put_Line ("Kind: " & Kind (T_Info)'Image);
      New_Line;
   end Check;

   Custom_Regexp : constant Regexp := Compile (".*\.json");
begin
   Check_Directories
     ("Defaults", Default_JSON_Filename_Regexp, (1 => +"subdir1"));
   Check_Directories
     ("No directory", Default_JSON_Filename_Regexp, (1 .. 0 => <>));
   Check_Directories
     ("Two dirs", Default_JSON_Filename_Regexp, (+"subdir1", +"subdir2"));
   Check_Directories ("Custom regexp", Custom_Regexp, (1 => +"subdir1"));

   Check_Directory ("Defaults", Default_JSON_Filename_Regexp, "subdir1");
   Check_Directory ("Custom regexp", Custom_Regexp, "subdir1");

   Put_Line ("Done.");
end Main;
