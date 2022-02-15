with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   procedure Parse (Filename : String);

   -----------
   -- Parse --
   -----------

   procedure Parse (Filename : String) is
      Defs : Definition_Maps.Map;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      begin
         Defs := Parse_Definition_File (Filename);
      exception
         when Exc : Syntax_Error =>
            Put_Line ("Syntax error: " & Exception_Message (Exc));
            return;
      end;

      Dump (Defs);
   end Parse;

   Files : File_Array_Access :=
     Create (+".").Read_Dir_Recursive (+".def", Files_Only);
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
   Put_Line ("Done.");
end Main;
