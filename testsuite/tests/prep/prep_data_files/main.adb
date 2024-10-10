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

      Put_Line ("Formatting as a new preprocessor file:");
      declare
         use type File_Config_Maps.Map;

         New_Filename : constant String := "new-" & Filename;
         F            : File_Type;
         New_Data     : Preprocessor_Data;

         Old_Default, New_Default : File_Config;
         Old_Mapping, New_Mapping : File_Config_Maps.Map;
      begin
         Write_Preprocessor_Data_File (Data, New_Filename);
         Open (F, In_File, New_Filename);
         loop
            begin
               declare
                  Line : constant String := Get_Line (F);
               begin
                  if Line = "" then
                     Put_Line ("|");
                  else
                     Put_Line ("| " & Line);
                  end if;
               end;
            exception
               when End_Error =>
                  exit;
            end;
         end loop;
         Close (F);

         --  Compare the original preprocessor data with the parsing of the new
         --  file.

         New_Data :=
           Parse_Preprocessor_Data_File
             (New_Filename, Create_Path ((1 .. 0 => <>)));

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
   Parse ("cannot-read-def-file.txt");
   Parse ("invalid-def-1.txt");
   Parse ("invalid-def-2.txt");
   Parse ("invalid-def-3.txt");
   Parse ("invalid-def-4.txt");
   Parse ("invalid-def-5.txt");
   Parse ("standard.txt");
   Parse ("no-default.txt");
   Parse ("unknown-switch.txt");
   Parse ("no-such-file.txt");
   Put_Line ("Done.");
end Main;
