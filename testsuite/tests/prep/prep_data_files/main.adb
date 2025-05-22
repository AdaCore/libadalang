with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   Path : constant Any_Path := Parse_Path ("defs");

   procedure Parse (Prefix : String);

   -----------
   -- Parse --
   -----------

   procedure Parse (Prefix : String) is
      Data     : Preprocessor_Data;
      Filename : constant String := Prefix & ".txt";
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

      declare
         use type File_Config_Maps.Map;

         Dirname      : constant String := "new-" & Prefix;
         New_Filename : constant String := Compose (Dirname, "prep.txt");
         New_Data     : Preprocessor_Data;

         Old_Default, New_Default : File_Config;
         Old_Mapping, New_Mapping : File_Config_Maps.Map;
      begin
         --  Make sure the output directory exists: it usually does not, but in
         --  case the test is ran manually for debugging, the directory may
         --  already exist.

         if not Exists (Dirname) then
            Create_Directory (Dirname);
         end if;
         Write_Preprocessor_Data_File (Data, New_Filename, Dirname);

         --  Compare the original preprocessor data with the parsing of the new
         --  file.

         New_Data :=
           Parse_Preprocessor_Data_File (New_Filename, Parse_Path (Dirname));

         Old_Default := Default_Config (Data);
         New_Default := Default_Config (New_Data);
         if Old_Default /= New_Default then
            Dump (New_Data);
            raise Program_Error with "unexpected default config";
         end if;

         Old_Mapping := File_Configs (Data);
         New_Mapping := File_Configs (New_Data);
         if Old_Mapping /= New_Mapping then
            Dump (New_Data);
            raise Program_Error with "unexpected config mapping";
         end if;
      end;
      New_Line;
   end Parse;

begin
   Parse ("cannot-read-def-file");
   Parse ("invalid-def-1");
   Parse ("invalid-def-2");
   Parse ("invalid-def-3");
   Parse ("invalid-def-4");
   Parse ("invalid-def-5");
   Parse ("standard");
   Parse ("no-default");
   Parse ("unknown-switch");
   Parse ("no-such-file");

   --  Check that exception messages include only file basenames

   Parse ("subdir/prep-data-1");
   Parse ("subdir/prep-data-2");
   Parse ("subdir/prep-data-3");
   Parse ("subdir/no-such-file");

   Put_Line ("Done.");
end Main;
