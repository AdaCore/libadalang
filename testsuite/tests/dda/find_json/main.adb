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

   procedure Check
     (Label : String; Name_Pattern : Regexp; Directories : Filename_Array);
   --  Run Load_From_Directories on the arguments and lookup T

   -----------
   -- Check --
   -----------

   procedure Check
     (Label : String; Name_Pattern : Regexp; Directories : Filename_Array)
   is
      Repinfo : Repinfo_Collection;
      T_Info  : Type_Representation;
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;

      begin
         Repinfo := Load_From_Directories (Name_Pattern, Directories);
      exception
         when Exc : Loading_Error =>
            Put_Line ("Loading_Error: " & Exception_Message (Exc));
            New_Line;
            return;
      end;

      T_Info := Repinfo.Lookup (T);
      if Is_Null (T_Info) then
         Put_Line ("Cannot find type representation of " & T.Image);
         New_Line;
         return;
      end if;

      Put_Line ("Kind: " & Kind (T_Info)'Image);
      New_Line;
   end Check;

begin
   Check ("Defaults", Default_JSON_Filename_Regexp, (1 => +"subdir1"));
   Check ("No directory", Default_JSON_Filename_Regexp, (1 .. 0 => <>));
   Check ("Two dirs", Default_JSON_Filename_Regexp, (+"subdir1", +"subdir2"));
   Check ("Custom regexp", Compile (".*\.json"), (1 => +"subdir1"));
   Put_Line ("Done.");
end Main;
