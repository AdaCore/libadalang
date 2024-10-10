with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Directories;       use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.Mmap;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure LAL_Prep is

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   procedure Error (Message : String) with No_Return;
   --  Print the given error message and exit with an non-zero status code

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Put_Line (Message);
      GNAT.OS_Lib.OS_Exit (1);
   end Error;

   function Starts_With (Value, Substring : String) return Boolean
   is (Value'Length >= Substring'Length
         and then
       Value (Value'First .. Value'First + Substring'Length - 1) = Substring);
   --  Return whether the first bytes of ``Value`` are equal to ``Substring``

   --  Decoded values for command-line arguments. Note that we do not use
   --  ``GNATCOLL.Opt_Parse`` because we need to process "-b" and "-c" options
   --  as affecting the same decoded value (-b sets it to one value, -c sets it
   --  to another value), which is not possible with ``GNATCOLL.Opt_Parse``
   --  right now.

   package Args is
      Line_Mode        : Any_Line_Mode := Delete_Lines;
      Line_Mode_Passed : Boolean := False;

      Definitions : String_Vectors.Vector;
      Data_File   : Unbounded_String;

      Undefined_Is_False        : Boolean := False;
      Undefined_Is_False_Passed : Boolean := False;

      In_File : Unbounded_String;
   end Args;

   Cfg         : File_Config;
   Contents    : Preprocessed_Source;
   Diagnostics : Diagnostics_Vectors.Vector;
   I           : Positive := 1;

begin
   --  Parse command-line arguments

   while I <= Argument_Count loop
      declare
         Arg : constant String := Argument (I);
      begin
         if Arg = "-b" then
            Args.Line_Mode := Blank_Lines;
            Args.Line_Mode_Passed := True;
         elsif Arg = "-c" then
            Args.Line_Mode := Comment_Lines;
            Args.Line_Mode_Passed := True;
         elsif Arg = "-u" then
            Args.Undefined_Is_False := True;
            Args.Undefined_Is_False_Passed := True;

         elsif Starts_With (Arg, "-D") then
            Args.Definitions.Append
              (To_Unbounded_String (Arg (Arg'First + 2 ..  Arg'Last)));

         elsif Starts_With (Arg, "-d") then
            if Arg = "-d" then
               I := I + 1;
               if I > Argument_Count then
                  Error ("Value missing for -d");
               end if;
               Args.Data_File := To_Unbounded_String (Argument (I));
            else
               Args.Data_File := To_Unbounded_String (Arg);
            end if;

         elsif Starts_With (Arg, "-") then
            Error ("Invalid option: " & Arg);

         elsif Length (Args.In_File) = 0 then
            Args.In_File := To_Unbounded_String (Arg);
         else
            Error ("Too many positional arguments");
         end if;
      end;

      I := I + 1;
   end loop;

   --  If we are given a preprocessing data file, load it and get the
   --  configuration for the source to preprocess.

   declare
      Data_File : constant String := To_String (Args.Data_File);
   begin
      if Data_File /= "" then
         declare
            use File_Config_Maps;

            Path           : constant Any_Path := Parse_Path
              (Ada.Environment_Variables.Value ("ADA_INCLUDE_PATH", ""));
            Default_Config : File_Config;
            File_Configs   : File_Config_Maps.Map;
            Basename       : Unbounded_String;
            Cur            : Cursor;
         begin
            Parse_Preprocessor_Data_File
              (Data_File, Path, Default_Config, File_Configs);
            Basename := To_Unbounded_String
              (Simple_Name (To_String (Args.In_File)));
            Cur := File_Configs.Find (Basename);
            Cfg := (if Has_Element (Cur)
                    then Element (Cur)
                    else Default_Config);
         end;
      end if;
   end;

   --  Regardless of what the data file said, force preprocessing on this file

   if not Cfg.Enabled then
      Cfg := (Enabled => True, others => <>);
   end if;

   --  Update the configuration from other options

   if Args.Line_Mode_Passed then
      Cfg.Line_Mode := Args.Line_Mode;
   end if;

   for Def of Args.Definitions loop
      declare
         Name  : Unbounded_String;
         Value : Value_Type;
      begin
         Parse_Definition_Option
           (To_String (Def), Name, Value, Empty_Valid => True);
         Cfg.Definitions.Include (Name, Value);
      end;
   end loop;

   if Args.Undefined_Is_False_Passed then
      Cfg.Undefined_Is_False := Args.Undefined_Is_False;
   end if;

   --  Finally read and preprocess the input source file

   declare
      use GNATCOLL.Mmap;

      File   : Mapped_File := Open_Read (To_String (Args.In_File));
      Region : Mapped_Region := Read (File);
      Buffer : constant Str_Access := Data (Region);
      Input  : String renames Buffer (1 .. Last (Region));
   begin
      Preprocess (Cfg, Input, Contents, Diagnostics);
      Free (Region);
      Close (File);
   end;

   --  Print diagnostics

   for D of Diagnostics loop
      Put_Line (To_Pretty_String (D));
   end loop;

   --  Print the output buffer. If the last byte is a LF, do not emit it, as
   --  Ada.Text_IO will automatically insert it anyway.

   declare
      Last : constant Natural :=
        (if Contents.Last > 0
            and then Contents.Buffer (Contents.Last) = ASCII.LF
         then Contents.Last - 1
         else Contents.Last);
   begin
      Put_Line (Contents.Buffer (1 .. Last));
   end;

   Free (Contents);
end LAL_Prep;
