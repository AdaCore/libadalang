with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   Path : constant Any_Path := Parse_Path ("defs");

   procedure Parse (Filename : String);

   -----------
   -- Parse --
   -----------

   procedure Parse (Filename : String) is
      Data : Preprocessor_Data;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      begin
         Data := Parse_Preprocessor_Data_File (Filename, Path);
      exception
         when Exc : Syntax_Error | File_Read_Error =>
            Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
            return;
      end;

      Dump (Data);
   end Parse;

   Files : File_Array_Access :=
     Create (+".").Read_Dir_Recursive (+".txt", Files_Only);
begin
   Sort (Files.all);
   for F of Files.all loop
      declare
         Filename : constant String := +F.Base_Name;
      begin
         Parse (Filename);
         New_Line;
      end;
   end loop;
   Unchecked_Free (Files);
   Parse ("no-such-file.txt");
   Put_Line ("Done.");
end Main;
