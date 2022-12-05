with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;

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
            New_Line;
            return;
      end;

      Dump (Data);
      New_Line;
   end Parse;

begin
   Parse ("cannot-read-def-file.txt");
   Parse ("invalid-def-1.txt");
   Parse ("invalid-def-2.txt");
   Parse ("invalid-def-3.txt");
   Parse ("standard.txt");
   Parse ("unknown-switch.txt");
   Parse ("no-such-file.txt");
   Put_Line ("Done.");
end Main;
