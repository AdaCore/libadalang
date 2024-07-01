--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Generic_Array_Sort;
with Ada.Directories;         use Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Strings;  use GNATCOLL.Strings;

with Langkit_Support.Slocs;  use Langkit_Support.Slocs;
with Langkit_Support.Text;   use Langkit_Support.Text;

with Libadalang.Common;           use Libadalang.Common;
with Libadalang.GPR_Utils;        use Libadalang.GPR_Utils;
with Libadalang.PP_Impl;          use Libadalang.PP_Impl;
with Libadalang.PP_Lexer;         use Libadalang.PP_Lexer;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

package body Libadalang.Preprocessing is

   procedure Free is new Ada.Unchecked_Deallocation
     (Preprocessor_Data_Record, Preprocessor_Data_Access);

   type Dummy_Unit_Provider is new Unit_Provider_Interface with null record;
   --  Dummy provider for ``Preprocessor_Data_Record.Context``. The default
   --  provider calls GPR code, which is costly for nothing: this context is
   --  used only to do parsing, so there is no unit resolution.

   overriding function Get_Unit_Filename
     (Self : Dummy_Unit_Provider;
      Name : Text_Type;
      Kind : Analysis_Unit_Kind) return String is (raise Program_Error);

   overriding function Get_Unit
     (Self    : Dummy_Unit_Provider;
      Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit'Class
   is (raise Program_Error);

   overriding procedure Release (Self : in out Dummy_Unit_Provider) is null;

   function Create_Dummy_Context return Analysis_Context
   is
     (Create_Context (Unit_Provider => Create_Unit_Provider_Reference
                                         (Dummy_Unit_Provider'(null record))));

   type Pp_File_Reader is new File_Reader_Interface with record
      Data : Preprocessor_Data;
      --  Preprocessor data to use when reading source files
   end record;
   --  File reader to preprocess source files

   overriding procedure Read
     (Self        : Pp_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out Pp_File_Reader) is null;

   type File_Config_Acc is access constant File_Config;

   function Lookup_Config
     (Data     : Preprocessor_Data;
      Filename : String) return not null File_Config_Acc;
   --  Look for the file config to use for a given Ada source file.
   --  ``Filename`` does not need to be valid from the current working
   --  directory as only the base name is used.

   function Lookup_File (Path : Any_Path; Filename : String) return String;
   --  Wrapper around ``GNATCOLL.File_Paths.Lookup`` to raise a
   --  ``File_Read_Error`` exception when the file is not found.

   function Parse_Definition_File
     (Diagnostic_Filename, Filename : String) return Definition_Maps.Map;
   --  Wrapper around the public function, letting the caller to specify a
   --  diagnostic filename.

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Any_Tree;
      View           : Any_View;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map)
      with Pre => Tree.Kind = View.Kind;
   --  Common implementation for the homonym public functions

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : Pp_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      In_Buffer  : String_Access;
      Out_Buffer : Preprocessed_Source;
   begin
      --  Just perform a direct read if preprocessing is disabled for this
      --  source file.

      if not Needs_Preprocessing (Self.Data, Filename) then
         Direct_Read (Filename, Charset, Read_BOM, Contents, Diagnostics);
         return;
      end if;

      --  According to the file reader interface, if there is at least one
      --  diagnostic, the value of ``Contents`` is ignored, so it is ok to
      --  leave it uninitialized on error.
      --
      --  First, read the file content to preprocess.

      begin
         In_Buffer := Read (Filename);
      exception
         when Exc : File_Read_Error =>
            Append (Diagnostics, Exc => Exc);
            Contents := Create_Decoded_File_Contents ("");
            return;
      end;

      --  Run the preprocessor on it and free the temporary buffer

      Preprocess (Lookup_Config (Self.Data, Filename).all,
                  Self.Data.Data.Context,
                  In_Buffer.all,
                  Out_Buffer,
                  Diagnostics);
      Free (In_Buffer);

      --  The preprocessor is always supposed to return a (possibly empty)
      --  source buffer.

      pragma Assert (Out_Buffer.Buffer /= null);
      Decode_Buffer
        (Out_Buffer.Buffer (1 .. Out_Buffer.Last),
         Charset,
         Read_BOM,
         Contents,
         Diagnostics);
      Free (Out_Buffer.Buffer);
   end Read;

   -------------------
   -- Lookup_Config --
   -------------------

   function Lookup_Config
     (Data     : Preprocessor_Data;
      Filename : String) return not null File_Config_Acc
   is
      use File_Config_Maps;

      Key : constant US.Unbounded_String :=
        US.To_Unbounded_String (Simple_Name (Filename));
      Cur : constant Cursor := Data.Data.File_Configs.Find (Key);
   begin
      if Has_Element (Cur) then
         declare
            Result : File_Config renames
              Data.Data.File_Configs.Reference (Cur);
         begin
            return Result'Unrestricted_Access;
         end;
      else
         return Data.Data.Default_Config'Unrestricted_Access;
      end if;
   end Lookup_Config;

   -----------------
   -- Lookup_File --
   -----------------

   function Lookup_File (Path : Any_Path; Filename : String) return String is
   begin
      return Result : constant String := Lookup (Path, Filename) do
         if Result = "" then
            raise File_Read_Error with "no such file: " & Filename;
         end if;
      end return;
   end Lookup_File;

   ---------------
   -- As_String --
   ---------------

   function As_String (Value : Value_Type) return US.Unbounded_String is
   begin
      return (case Value.Kind is
              when Empty          => US.Null_Unbounded_String,
              when String_Literal => Value.String_Value,
              when Symbol         => Value.Symbol_Value);
   end As_String;

   -----------------------------
   -- Parse_Definition_Option --
   -----------------------------

   procedure Parse_Definition_Option
     (Option : String; Name : out US.Unbounded_String; Value : out Value_Type)
   is
      --  We accept either "<X>=<Y>", where X is a valid identifier and Y may
      --  be empty, or just "<X>".

      Eq : constant Natural :=
        Ada.Strings.Fixed.Index (Option, "=", Option'First);

      Name_Last : constant Positive := (if Eq = 0
                                        then Option'Last
                                        else Eq - 1);
      Value_First : constant Positive := (if Eq = 0
                                          then Option'Last + 1
                                          else Eq + 1);

      N : String renames Option (Option'First .. Name_Last);
      V : String renames Option (Value_First .. Option'Last);
   begin
      if Is_Valid_Identifier (N) then
         Name := US.To_Unbounded_String (To_Lower (N));
      else
         raise Syntax_Error with "invalid preprocessing symbol name: " & N;
      end if;

      if V = "" then
         Value := (Kind => Empty);

      elsif V (V'First) = '"' then
         Value := (Kind         => String_Literal,
                   String_Value => US.To_Unbounded_String (V));

      else
         Value := (Kind         => Symbol,
                   Symbol_Value => US.To_Unbounded_String (V));
      end if;
   end Parse_Definition_Option;

   ---------------------------
   -- Parse_Definition_File --
   ---------------------------

   function Parse_Definition_File
     (Diagnostic_Filename, Filename : String) return Definition_Maps.Map
   is
      L : Lexer := Create_Lexer (Diagnostic_Filename, Filename);
      T : Token_Type;

      procedure Error (Message : String) with No_Return;
      --  Raise a ``Syntax_Error`` exceptions with the given error message,
      --  including sloc information from ``T``.

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         raise Syntax_Error with
           Filename & ":" & Image (T.Sloc) & ": " & Message;
      end Error;

      Result : Definition_Maps.Map;

      Sym : US.Unbounded_String;
      Val : Value_Type;
      --  Temporaries to track the symbol/value of a definition during parsing
   begin
      --  Scan all lines until reaching the end of file

      Scan_Lines : loop
         Next (L, T);

         case T.Kind is
            when Chars_Sequence =>
               --  Try to get a symbol/value association

               if not Is_Valid_Identifier (L, T) then
                  Error ("symbol expected");
               end if;
               Sym := Get_Text_Lowercase (L, T);

               Next (L, T);
               if T.Kind /= Assign then
                  Error ("assignment operator expected");
               end if;

               --  The value is optional, so it is legitimate to reach the end
               --  of line here.

               Next (L, T);
               case T.Kind is
                  when EOF | EOL =>
                     Val := (Kind => Empty);

                  when String_Literal =>
                     Val := (Kind         => String_Literal,
                             String_Value => Get_Text (L, T));

                  when Chars_Sequence =>
                     Val := (Kind => Symbol, Symbol_Value => Get_Text (L, T));

                  when others =>
                     Error ("value or end of line expected");
               end case;

               Result.Include (Sym, Val);

               --  The symbol/value association is complete: make sure we now
               --  reach the end of line (to the next association) or the end
               --  of file (parsing completed).

               if T.Kind not in EOF | EOL then
                  Next (L, T);
                  if T.Kind = EOF then
                     exit Scan_Lines;
                  elsif T.Kind /= EOL then
                     Error ("end of line expected");
                  end if;
               end if;

            when EOL =>
               null;

            when EOF =>
               exit Scan_Lines;

            when others =>
               Error ("symbol expected");
         end case;
      end loop Scan_Lines;

      return Result;
   end Parse_Definition_File;

   ---------------------------
   -- Parse_Definition_File --
   ---------------------------

   function Parse_Definition_File
     (Filename : String) return Definition_Maps.Map
   is
   begin
      return Parse_Definition_File (Filename, Filename);
   end Parse_Definition_File;

   ----------
   -- Move --
   ----------

   procedure Move (Target, Source : in out File_Config) is
      use type System.Address;
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Enabled then
         Target :=
           (Enabled            => True,
            Definitions        => <>,
            Line_Mode          => Source.Line_Mode,
            Print_Symbols      => Source.Print_Symbols,
            Undefined_Is_False => Source.Undefined_Is_False);
         Target.Definitions.Move (Source.Definitions);
         Source := Disabled_File_Config;
      else
         Target := Source;
      end if;
   end Move;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Preprocessor_Data) return Boolean is
      use type File_Config_Maps.Map;
   begin
      if Left.Data = null or else Right.Data = null then
         return Left.Data = Right.Data;
      else
         return Left.Default_Config = Right.Default_Config
                and then Left.File_Configs = Right.File_Configs;
      end if;
   end "=";

   ----------------------------------
   -- Parse_Preprocessor_Data_File --
   ----------------------------------

   function Parse_Preprocessor_Data_File
     (Filename : String; Path : Any_Path) return Preprocessor_Data
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;
   begin
      Parse_Preprocessor_Data_File
        (Filename, Path, Default_Config, File_Configs);
      return Create_Preprocessor_Data (Default_Config, File_Configs);
   end Parse_Preprocessor_Data_File;

   ----------------------------------
   -- Parse_Preprocessor_Data_File --
   ----------------------------------

   procedure Parse_Preprocessor_Data_File
     (Filename       : String;
      Path           : Any_Path;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map)
   is
      L : Lexer := Create_Lexer (Filename, Lookup_File (Path, Filename));
      T : Token_Type;

      procedure Error (Message : String) with No_Return;

      procedure Process_Switch (Cfg : in out File_Config; Switch : String);
      --  Decode the ``Switch`` gnatprep command line switch and update ``Cfg``
      --  accordingly. Raise a ``Syntax_Error`` if the switch is invalid.

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         raise Syntax_Error with
           Filename & ":" & Image (T.Sloc) & ": " & Message;
      end Error;

      --------------------
      -- Process_Switch --
      --------------------

      procedure Process_Switch (Cfg : in out File_Config; Switch : String) is
      begin
         if Switch = "-b" then
            Cfg.Line_Mode := Blank_Lines;

         elsif Switch = "-c" then
            Cfg.Line_Mode := Comment_Lines;

         elsif Switch = "-s" then
            Cfg.Print_Symbols := True;

         elsif Switch = "-u" then
            Cfg.Undefined_Is_False := True;

         elsif Switch (Switch'First .. Switch'First + 1) = "-D" then
            declare
               Name  : US.Unbounded_String;
               Value : Value_Type;
            begin
               Parse_Definition_Option
                 (Switch (Switch'First + 2 .. Switch'Last),
                  Name, Value);
               Cfg.Definitions.Include (Name, Value);
            exception
               when Exc : Syntax_Error =>
                  Error ("for option -D: " & Exception_Message (Exc));
            end;

         else
            Error ("invalid switch: " & Switch);
         end if;
      end Process_Switch;

      File    : US.Unbounded_String;
      Default : Boolean;
      Cfg     : File_Config;
   begin
      Default_Config := Disabled_File_Config;
      File_Configs := File_Config_Maps.Empty_Map;

      --  Scan all lines until reaching the end of file

      Scan_Lines : loop
         Next (L, T);

         case T.Kind is
            when Star | String_Literal =>
               Cfg := Disabled_File_Config;

               --  Try to get a file configuration: first a file name for an
               --  Ada source.

               Default := T.Kind = Star;
               if not Default then
                  File := Denoted_String (L, T);
               end if;

               --  Then an optional file

               Next (L, T);
               if T.Kind = String_Literal then
                  declare
                     Base_Name : constant String :=
                       US.To_String (Denoted_String (L, T));
                     Full_Name : constant String :=
                       Lookup_File (Path, Base_Name);
                  begin
                     Cfg := Base_Enabled_File_Config;
                     Cfg.Definitions := Parse_Definition_File (Full_Name);
                     Next (L, T);
                  end;
               end if;

               --  Finally, an optional list of command-line options

               while T.Kind not in EOL | EOF loop
                  if T.Kind = Switch then
                     if not Cfg.Enabled then
                        Cfg := Base_Enabled_File_Config;
                     end if;
                     Process_Switch (Cfg, US.To_String (Get_Text (L, T)));
                  else
                     Error ("switch expected");
                  end if;
                  Next (L, T);
               end loop;

               if Default then
                  Default_Config := Cfg;
               else
                  File_Configs.Include (File, Cfg);
               end if;

            when EOL =>
               null;

            when EOF =>
               exit Scan_Lines;

            when others =>
               Error ("Ada source filename expected");
         end case;
      end loop Scan_Lines;
   end Parse_Preprocessor_Data_File;

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Any_Tree;
      View           : Any_View;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map)
   is
      Defs : Definition_Maps.Map;
      --  Keep track of the definitions

      Prep_Data_File_Found : Boolean := False;
      --  Whether we have found a preprocessor data file

      --  Loading preprocessor data is done in two steps: first load the
      --  preprocessor data file (if any), then use the symbol definitions
      --  found as compilation options. This matches what GNAT implements:
      --  ``-gnateDX=Y -gnatep=foo.txt`` sets the ``X`` symbol to ``Y`` even if
      --  ``foo.txt`` contains another symbol definition for ``X``.

      procedure Process_Switch (View : Any_View; Switch : XString);
      --  If relevant, add the compiler ``Switch`` (found in ``View``) to our
      --  knowledge base: the ``Default_Config`` and ``File_Configs`` arguments
      --  (for preprocessor data files) and ``Defs`` (for symbol definitions).

      Source_Dirs : constant Any_Path := Source_Dirs_Path (Tree, View);
      --  Source directories in which to look for preprocessor data files

      --------------------
      -- Process_Switch --
      --------------------

      procedure Process_Switch (View : Any_View; Switch : XString)
      is
         --  Prefixes for the command-line options to match

         Def_Prefix  : constant String := "-gnateD";
         File_Prefix : constant String := "-gnatep=";
      begin
         --  If this option defines a symbol, add it to our list of symbols

         if Switch.Starts_With (Def_Prefix) then
            declare
               Option : constant XString :=
                 Switch.Slice (Def_Prefix'Length + 1, Switch.Length);
               Name   : US.Unbounded_String;
               Value  : Value_Type;
            begin
               Parse_Definition_Option (Option.To_String, Name, Value);
               Defs.Include (Name, Value);
            end;

         --  If this is the first option we see that uses a preprocessor data
         --  file, load it.

         elsif Switch.Starts_With (File_Prefix)
               and then not Prep_Data_File_Found
         then
            declare
               File : constant XString :=
                 Switch.Slice (File_Prefix'Length + 1, Switch.Length);
               --  Name of the preprocessor data file. It may appear absolute
               --  or relative in the project file.

               Path : Any_Path := Source_Dirs;
               --  Preprocessor data files are looked up in the object
               --  directory or in any of the source directories in the project
               --  tree.
            begin
               Add_Directory (Path, Object_Dir (View));
               Parse_Preprocessor_Data_File
                 (File.To_String, Path, Default_Config, File_Configs);
            end;
            Prep_Data_File_Found := True;
         end if;
      end Process_Switch;

   begin
      Default_Config := Disabled_File_Config;
      File_Configs := File_Config_Maps.Empty_Map;

      --  Go through all subprojects and extract preprocessor data from their
      --  compiler switches.

      Iterate_Ada_Compiler_Switches (Tree, View, Process_Switch'Access);

      --  Now that we have potentially found a preprocessor data file, complete
      --  preprocessor data with the symbol definitions we have found.

      if not Defs.Is_Empty then
         declare
            procedure Process (Config : in out File_Config);
            --  Make sure preprocessing is enabled for ``Config`` and add
            --  symbol definitions from ``Defs`` to it.

            -------------
            -- Process --
            -------------

            procedure Process (Config : in out File_Config) is
               use Definition_Maps;
            begin
               if not Config.Enabled then
                  Config := Base_Enabled_File_Config;
               end if;
               for Cur in Defs.Iterate loop
                  Config.Definitions.Include (Key (Cur), Element (Cur));
               end loop;
            end Process;
         begin
            Iterate (Default_Config, File_Configs, Process'Access);
         end;
      end if;
   end Extract_Preprocessor_Data_From_Project;

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   function Extract_Preprocessor_Data_From_Project
     (Tree    : Prj.Project_Tree'Class;
      Project : Prj.Project_Type := Prj.No_Project) return Preprocessor_Data
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;
   begin
      Extract_Preprocessor_Data_From_Project
        (Tree, Project, Default_Config, File_Configs);
      return Create_Preprocessor_Data (Default_Config, File_Configs);
   end Extract_Preprocessor_Data_From_Project;

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Prj.Project_Tree'Class;
      Project        : Prj.Project_Type := Prj.No_Project;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map) is
   begin
      Extract_Preprocessor_Data_From_Project
        (Tree           => (Kind       => GPR1_Kind,
                            GPR1_Value => Tree'Unrestricted_Access),
         View           => (Kind       => GPR1_Kind,
                            GPR1_Value => Project),
         Default_Config => Default_Config,
         File_Configs   => File_Configs);
   end Extract_Preprocessor_Data_From_Project;

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   function Extract_Preprocessor_Data_From_Project
     (Tree    : GPR2.Project.Tree.Object;
      Project : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return Preprocessor_Data
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;
   begin
      Extract_Preprocessor_Data_From_Project
        (Tree, Project, Default_Config, File_Configs);
      return Create_Preprocessor_Data (Default_Config, File_Configs);
   end Extract_Preprocessor_Data_From_Project;

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : GPR2.Project.Tree.Object;
      Project        : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map) is
   begin
      Check_Source_Info (Tree);

      Extract_Preprocessor_Data_From_Project
        (Tree           => (Kind => GPR2_Kind, GPR2_Value => Tree),
         View           => (Kind => GPR2_Kind, GPR2_Value => Project),
         Default_Config => Default_Config,
         File_Configs   => File_Configs);
   end Extract_Preprocessor_Data_From_Project;

   ------------------------------
   -- Create_Preprocessor_Data --
   ------------------------------

   function Create_Preprocessor_Data
     (Default_Config : in out File_Config;
      File_Configs   : in out File_Config_Maps.Map) return Preprocessor_Data
   is
      R : constant Preprocessor_Data_Access :=
        new Preprocessor_Data_Record'
          (Ref_Count      => 1,
           Default_Config => <>,
           File_Configs   => <>,
           Context        => Create_Dummy_Context);
   begin
      Move (R.Default_Config, Default_Config);
      R.File_Configs.Move (File_Configs);
      return (Ada.Finalization.Controlled with Data => R);
   end Create_Preprocessor_Data;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Default_Config : in out File_Config;
      File_Configs   : in out File_Config_Maps.Map;
      Process        : access procedure (Config : in out File_Config))
   is
   begin
      Process.all (Default_Config);
      for Config of File_Configs loop
         Process.all (Config);
      end loop;
   end Iterate;

   --------------------
   -- Default_Config --
   --------------------

   function Default_Config (Data : Preprocessor_Data) return File_Config is
   begin
      return Data.Data.Default_Config;
   end Default_Config;

   ------------------
   -- File_Configs --
   ------------------

   function File_Configs
     (Data : Preprocessor_Data) return File_Config_Maps.Map is
   begin
      return Data.Data.File_Configs;
   end File_Configs;

   -------------------------
   -- Needs_Preprocessing --
   -------------------------

   function Needs_Preprocessing
     (Data     : Preprocessor_Data;
      Filename : String) return Boolean is
   begin
      return Lookup_Config (Data, Filename).Enabled;
   end Needs_Preprocessing;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Preprocessed_Source) is
   begin
      Free (Self.Buffer);
      Self.Last := 0;
   end Free;

   ----------------
   -- Preprocess --
   ----------------

   procedure Preprocess
     (Config      : File_Config;
      Input       : String;
      Contents    : out Preprocessed_Source;
      Diagnostics : in out Diagnostics_Vectors.Vector) is
   begin
      Preprocess (Config, Create_Dummy_Context, Input, Contents, Diagnostics);
   end Preprocess;

   ----------------
   -- Preprocess --
   ----------------

   procedure Preprocess
     (Data            : Preprocessor_Data;
      Filename, Input : String;
      Contents        : out Preprocessed_Source;
      Diagnostics     : in out Diagnostics_Vectors.Vector) is
   begin
      Preprocess (Lookup_Config (Data, Filename).all,
                  Data.Data.Context,
                  Input,
                  Contents,
                  Diagnostics);
   end Preprocess;

   -------------------------
   -- Create_Preprocessor --
   -------------------------

   function Create_Preprocessor
     (Default_Config : in out File_Config;
      File_Configs   : in out File_Config_Maps.Map)
      return File_Reader_Reference is
   begin
      return Create_File_Reader_Reference
        (Pp_File_Reader'
          (File_Reader_Interface with
           Data => Create_Preprocessor_Data (Default_Config, File_Configs)));
   end Create_Preprocessor;

   -----------------------------------
   -- Create_Preprocessor_From_File --
   -----------------------------------

   function Create_Preprocessor_From_File
     (Filename : String; Path : Any_Path) return File_Reader_Reference is
   begin
      return Create_File_Reader_Reference
        (Pp_File_Reader'
          (File_Reader_Interface with
           Data => Parse_Preprocessor_Data_File (Filename, Path)));
   end Create_Preprocessor_From_File;

   -----------------------------------
   -- Create_Preprocessor_From_File --
   -----------------------------------

   function Create_Preprocessor_From_File
     (Filename  : String;
      Path      : Any_Path;
      Line_Mode : Any_Line_Mode) return File_Reader_Reference
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;

      procedure Process (Config : in out File_Config);
      --  If ``Config`` is enabled, force its line mode to ``Line_Mode``

      -------------
      -- Process --
      -------------

      procedure Process (Config : in out File_Config) is
      begin
         if Config.Enabled then
            Config.Line_Mode := Line_Mode;
         end if;
      end Process;

   begin
      Parse_Preprocessor_Data_File
        (Filename, Path, Default_Config, File_Configs);
      Iterate (Default_Config, File_Configs, Process'Access);

      return Create_Preprocessor (Default_Config, File_Configs);
   end Create_Preprocessor_From_File;

   ----------
   -- Dump --
   ----------

   procedure Dump (Definitions : Definition_Maps.Map; Prefix : String := "") is
      --  For output stability, write symbol definitions with symbol name order

      use Definition_Maps;

      type Cur_Array is array (Positive range <>) of Cursor;
      function "<" (Left, Right : Cursor) return Boolean
      is (US."<" (Key (Left), Key (Right)));
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Positive, Cursor, Cur_Array);

      Defs : Cur_Array (1 .. Natural (Definitions.Length));
      Cur  : Cursor := Definitions.First;
      V    : Value_Type;
   begin
      for D of Defs loop
         D := Cur;
         Cur := Next (Cur);
      end loop;
      Sort (Defs);

      for D of Defs loop
         Put (Prefix & US.To_String (Key (D)) & " -> ");
         V := Element (D);
         case V.Kind is
            when Empty =>
               Put_Line ("<null>");
            when String_Literal =>
               Put_Line ("String(" & US.To_String (V.String_Value) & ")");
            when Symbol =>
               Put_Line ("Symbol(" & US.To_String (V.Symbol_Value) & ")");
         end case;
      end loop;
      if Defs'Length = 0 then
         Put_Line (Prefix & "<none>");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Config : File_Config; Prefix : String := "") is
   begin
      if not Config.Enabled then
         Put_Line (Prefix & "  <disabled>");
         return;
      end if;

      case Config.Line_Mode is
         when Delete_Lines =>
            null;
         when Blank_Lines  =>
            Put_Line (Prefix & "  -b");
         when Comment_Lines  =>
            Put_Line (Prefix & "  -c");
      end case;
      if Config.Print_Symbols then
         Put_Line (Prefix & "  -s");
      end if;
      if Config.Undefined_Is_False then
         Put_Line (Prefix & "  -u");
      end if;

      Dump (Config.Definitions, Prefix & "  ");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Data : Preprocessor_Data; Prefix : String := "") is

      --  For output stability, write file configurations with file name order

      use File_Config_Maps;

      type Cur_Array is array (Positive range <>) of Cursor;
      function "<" (Left, Right : Cursor) return Boolean
      is (US."<" (Key (Left), Key (Right)));
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Positive, Cursor, Cur_Array);

      Cfgs  : File_Config_Maps.Map renames Data.Data.File_Configs;
      Files : Cur_Array (1 .. Natural (Cfgs.Length));
      Cur   : Cursor := Cfgs.First;
      First : Boolean := True;
   begin
      for F of Files loop
         F := Cur;
         Next (Cur);
      end loop;
      Sort (Files);

      Put_Line (Prefix & "# default");
      Dump (Data.Data.Default_Config, Prefix);
      Put_Line (Prefix);
      for Cur of Files loop
         if First then
            First := False;
         else
            Put_Line (Prefix);
         end if;
         Put_Line (Prefix & "# """ & US.To_String (Key (Cur)) & """");
         Dump (Element (Cur), Prefix);
      end loop;
   end Dump;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Preprocessor_Data) is
   begin
      if Self.Data /= null then
         Self.Data.Ref_Count := Self.Data.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Preprocessor_Data) is
   begin
      if Self.Data /= null then
         Self.Data.Ref_Count := Self.Data.Ref_Count - 1;
         if Self.Data.Ref_Count = 0 then
            Free (Self.Data);
         end if;
      end if;
   end Finalize;

end Libadalang.Preprocessing;
