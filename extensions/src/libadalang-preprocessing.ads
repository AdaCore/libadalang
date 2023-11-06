--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package implements a preprocessor for Ada sources that is compatible
--  with GNATprep. It also provides a file reader implementing such a
--  preprocessor, to be used in an analysis context. Please refer to GNATprep's
--  documentation for a description of the main concepts, of preprocessor data
--  files and of preprocessing features. The API provided here closely follows
--  these concepts.
--
--  The action of preprocessing an Ada source file is done according to
--  parameters (definition of preprocessor symbols, how to format directives
--  and disabled lines in the output, ...). The ``File_Config`` type is used to
--  represent such parameters, and the ``Preprocess`` procedure taking a
--  ``File_Config`` argument can be used to preprocess a given source buffer.
--
--  .. code-block:: ada
--
--     --  Create a file configuration using symbol definitions from GNATprep's
--     --  "foo.txt" definition file, replacing directives and disabled lines
--     --  with blank lines.
--
--     Cfg : constant File_Config :=
--       (Enabled => True,
--        Definitions => Parse_Definition_File ("foo.txt"),
--        Line_Mode   => Blank_Lines,
--        others      => <>);
--
--      Input_Buffer  : String := "...";
--      Output_Buffer : Preprocessed_Source;
--      Diagnostics   : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
--
--     --  Preprocess the "Input_Buffer" source, writing the result to
--     --  ``Output_Buffer``.
--
--     Preprocess (Cfg, Input_Buffer, Output_Buffer, Diagnostics);
--
--     if not Diagnostics.Is_Empty then
--        --  Raise some error
--
--     else
--        declare
--           Buffer : String renames
--             Output_Buffer.Buffer (1 .. Output_Buffer.Last)
--        begin
--           --  Use the preprocessed source in "Buffer"
--        end;
--     end if;
--
--  Preprocessing for a whole Ada project is determined by a set of file
--  configurations: optionally several ``File_Config`` values for sources with
--  specific file names (see the ``File_Config_Maps.Map`` type), plus an
--  additional ``File_Config`` value to use for files not described in this
--  map (the "default" file config).
--
--  One can either create these data structures by hand, or parsing GNATprep's
--  "preprocessor data file". In the latter case, the
--  ``Parse_Preprocessor_Data_File`` and ``Create_Preprocessor_Data``
--  functions will cover each case to create the final "aggregated"
--  configuration: a ``Preprocessor_Data`` value.
--
--  .. code-block:: ada
--
--     Path : constant Any_Path :=
--       Create_Path_From_Environ ("ADA_INCLUDE_PATH");
--     --  This path allows to find the preprocessor data file and the
--     --  definition files it references in the current directory or in any of
--     --  the directories pointed by the "ADA_INCLUDE_PATH" environment
--     --  variable.
--
--     Prep : constant Preprocessor_Data :=
--       Parse_Preprocessor_Data_File ("prep-data.txt", Path);
--     --  Parse the "prep-data.txt" preprocessor data file and create a full
--     --  preprocessor configuration from it.
--
--  From there, it is possible to call the "Preprocess" procedure taking a
--  "Preprocessor_Data" argument, plus the file name for the source file to
--  preprocess (used to look up the corresponding file configuration).
--
--  .. code-block:: ada
--
--     --  Preprocess the "Input_Buffer" source as being the content of a
--     --  "foo.adb" Ada source file, writing the result to "Output_Buffer".
--
--     Preprocess (Prep, "foo.adb", Input_Buffer, Output_Buffer, Diagnostics);
--
--     if not Diagnostics.Is_Empty then
--        --  Raise some error
--
--     else
--        declare
--           Buffer : String renames
--             Output_Buffer.Buffer (1 .. Output_Buffer.Last)
--        begin
--           --  Use the preprocessed source in "Buffer"
--        end;
--     end if;
--
--  Finally, in order to instruct a Libadalang analysis context to
--  automatically preprocess source files when loading files through the
--  ``Get_From_File`` function, one needs to use the file reader mechanism (see
--  ``Langkit_Support.File_Readers``): first create a ``File_Reader_Reference``
--  value that implements preprocessing (see the ``Create_Preprocessor`` and
--  ``Create_Preprocessor_From_File`` functions defined in this package) and
--  then pass it to the ``Create_Context`` context constructor.
--
--  .. code-block:: ada
--
--     FR  : constant File_Reader_Reference :=
--       Create_Preprocessor_From_File ("prep-data.txt", Path);
--     Ctx : constant Analysis_Context := Create_Context (File_Reader => FR);
--
--     --  Analyze the "foo.adb" source file after preprocessing it according
--     --  to configuration for "foo.adb" files in "prep-data.txt". The
--     --  analysis of any other source file that this implies will also
--     --  trigger preprocessing for these files.
--
--     U : constant Analysis_Unit := Ctx.Get_From_File ("foo.adb");

with Ada.Containers.Hashed_Maps;
private with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.Projects;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

private with Libadalang.Analysis;

package Libadalang.Preprocessing is

   package US renames Ada.Strings.Unbounded;
   package Prj renames GNATCOLL.Projects;

   --  All parsing functions below may raise two kinds of exceptions from
   --  ``Langkit_Support.Errors``:
   --
   --  * ``File_Read_Error`` when reading a preprocessor data file or a
   --    definition file failed.
   --
   --  * ``Syntax_Error`` when parsing a preprocessor data file or a
   --    definition file failed.

   ---------------------------------
   --  Preprocessor configuration --
   ---------------------------------

   --  The GNAT preprocessor operates on byte streams: dealing with non-ASCII
   --  symbols/values requires going through encodings such as ISO-8859-1, i.e.
   --  which have a bijection between bytes and codepoints covering all bytes.

   type Value_Kind is (Empty, String_Literal, Symbol);
   type Value_Type (Kind : Value_Kind := Empty) is record
      case Kind is
         when Empty          => null;
         when String_Literal => String_Value : US.Unbounded_String;
         when Symbol         => Symbol_Value : US.Unbounded_String;
      end case;
   end record;

   function As_String (Value : Value_Type) return US.Unbounded_String;
   --  Return the string that ``Value`` must substitute to in the preprocessed
   --  sources.

   procedure Parse_Definition_Option
     (Option : String; Name : out US.Unbounded_String; Value : out Value_Type);
   --  If ``Option`` matches ``<name>=<value>``, where ``<name>`` is a valid
   --  preprocessor symbol name, set ``Name`` and ``Value`` to the
   --  corresponding values.
   --
   --  If it matches ``<name>`` only, set ``Name`` to it and ``Value`` to the
   --  empty value.
   --
   --  Otherwise, raise a ``Syntax_Error`` exception.

   package Definition_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => Value_Type,
      Hash            => US.Hash,
      Equivalent_Keys => US."=");

   function Parse_Definition_File
     (Filename : String) return Definition_Maps.Map;
   --  Parse the symbol file at ``Filename`` and return the corresponding
   --  definitions.
   --
   --  See GNATprep's documentation for a description of this file.

   type Any_Line_Mode is (Delete_Lines, Blank_Lines, Comment_Lines);
   --  Determine how the preprocessor treats directives and disabled lines in
   --  the output.
   --
   --  ``Delete_Lines``
   --
   --    Just delete these lines: this breaks line number correspondance
   --    between the original source and the preprocessed one. This corresponds
   --    to GNATprep's default mode.
   --
   --  ``Blank_Lines``
   --
   --    Replace these lines with empty lines. This corresponds to GNATprep's
   --    ``-b`` option.
   --
   --  ``Comment_Lines``
   --
   --    Preserve these lines and emit a ``--!`` comment marker in front of
   --    them. This corresponds to GNATprep's ``-c`` option.

   type File_Config (Enabled : Boolean := False) is record
      case Enabled is
         when False => null;
         when True =>
            Definitions : Definition_Maps.Map;
            --  Symbol/value associations for this file. Note that, in order
            --  for the preprocessing to work correctly, symbols must be lower
            --  case.

            Line_Mode : Any_Line_Mode := Delete_Lines;
            --  Determine how the preprocessor treats directives and disabled
            --  lines in the output.

            Print_Symbols : Boolean := False;
            --  Whether to print a sorted list of symbol and values on the
            --  standard output. Actually unused in this module.

            Undefined_Is_False : Boolean := False;
            --  Whether to treat undefined symbols as False in the context of a
            --  preprocessor test (see GNATprep's ``-u`` option).
      end case;
   end record;

   Disabled_File_Config : constant File_Config := (Enabled => False);
   --  By default, the preprocessor is disabled on all Ada sources

   Base_Enabled_File_Config : constant File_Config :=
     (Enabled   => True,
      Line_Mode => Blank_Lines,
      others    => <>);
   --  Default file configuration when enabling preprocessing for a source
   --  file for the preprocessor integrated into GNAT.

   procedure Move (Target, Source : in out File_Config);
   --  Move data from ``Source`` to ``Target``. When this procedure returns
   --  ``Source`` is ``Disabled_File_Config``.

   package File_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => File_Config,
      Hash            => US.Hash,
      Equivalent_Keys => US."=");
   --  For each source file (identifier by basename only), preprocessor
   --  configuration to use.

   type Preprocessor_Data is private;
   --  File-specific Symbol/value associations and options to run the
   --  preprocessor.
   --
   --  This type is a reference to constant preprocessing configuration:
   --  copying this object is cheap.

   No_Preprocessor_Data : constant Preprocessor_Data;
   --  No reference to preprocessor data

   overriding function "=" (Left, Right : Preprocessor_Data) return Boolean;

   function Parse_Preprocessor_Data_File
     (Filename : String; Path : Any_Path) return Preprocessor_Data;
   --  Parse the preprocessor data file at ``Filename`` and return the
   --  corresponding data.
   --
   --  ``Path`` is used to look for the preprocessor data file itself and for
   --  definition files that the preprocessor data file may refer to.
   --
   --  See GNATprep's documentation for a description of the preprocessor data
   --  file format.

   procedure Parse_Preprocessor_Data_File
     (Filename       : String;
      Path           : Any_Path;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map);
   --  Like the ``Parse_Preprocessor_Data_File`` function, but instead fill out
   --  the ``Default_Config`` and ``File_Configs`` arguments. This procedure is
   --  useful in order to modify the parsed configuration before creating the
   --  ``Preprocessor_Data`` object.
   --
   --  See GNATprep's documentation for a description of the preprocessor data
   --  file format.

   function Extract_Preprocessor_Data_From_Project
     (Tree    : Prj.Project_Tree'Class;
      Project : Prj.Project_Type := Prj.No_Project) return Preprocessor_Data;
   --  Create preprocessor data from compiler arguments found in the given GPR
   --  project (``-gnatep`` and ``-gnateD`` arguments).
   --
   --  If a non-null ``Project`` is given, look for compiler arguments in it
   --  and the other projects in its closure.  If ``Project`` is left to
   --  ``No_Project``, try to use the whole project tree.
   --
   --  Note that this function collects all arguments and returns an
   --  approximation from them: it does not replicates exactly gprbuild's
   --  behavior.

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Prj.Project_Tree'Class;
      Project        : Prj.Project_Type := Prj.No_Project;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map);
   --  Like the ``Extract_Preprocessor_Data_From_Project`` function, but
   --  instead fill out the ``Default_Config`` and ``File_Configs`` arguments.
   --  This procedure is useful in order to modify the parsed configuration
   --  before creating the ``Preprocessor_Data`` object.

   function Extract_Preprocessor_Data_From_Project
     (Tree    : GPR2.Project.Tree.Object;
      Project : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return Preprocessor_Data;
   --  Likewise, but with GPR2 projects

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : GPR2.Project.Tree.Object;
      Project        : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map);
   --  Likewise, but with GPR2 projects

   function Create_Preprocessor_Data
     (Default_Config : in out File_Config;
      File_Configs   : in out File_Config_Maps.Map) return Preprocessor_Data;
   --  Create preprocessor data using the given file-specific configurations
   --  and the given default configuration (for other files).
   --
   --  Note that this "consumes" both arguments, which are left respectively to
   --  ``File_Config_Maps.Empty`` and ``Disabled_File_Config`` upon return.

   procedure Iterate
     (Default_Config : in out File_Config;
      File_Configs   : in out File_Config_Maps.Map;
      Process        : access procedure (Config : in out File_Config));
   --  Call ``Process`` on all the file configurations passed as arguments.
   --  This procedure helps forcing some configuration, for instance, forcing
   --  the line mode for all configurations.

   function Default_Config (Data : Preprocessor_Data) return File_Config;
   --  Return the default file configuration in ``Data``

   function File_Configs
     (Data : Preprocessor_Data) return File_Config_Maps.Map;
   --  Return all file configurations in ``Data``

   function Needs_Preprocessing
     (Data : Preprocessor_Data; Filename : String) return Boolean;
   --  Return whether ``Filename`` must be preprocessed according to ``Data``

   ------------------------------
   -- Preprocessing procedures --
   ------------------------------

   type Preprocessed_Source is record
      Buffer : String_Access;
      Last   : Natural;
   end record;
   --  Buffer that contains preprocessed sources. ``Buffer'First`` should be 1,
   --  and the actual content lies in ``Buffer (1 .. Last)``.

   procedure Free (Self : in out Preprocessed_Source);
   --  Deallocate the given source buffer

   procedure Preprocess
     (Config      : File_Config;
      Input       : String;
      Contents    : out Preprocessed_Source;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Preprocess the ``Input`` source buffer according to the given ``Config``
   --  preprocessor file configuration.
   --
   --  On success, leave ``Diagnostics`` empty and return in ``Contents`` a
   --  newly allocated string containing the preprocessed source.
   --
   --  On failure, leave ``Contents`` uninitialized and put error messages in
   --  ``Diagnostics``.

   procedure Preprocess
     (Data            : Preprocessor_Data;
      Filename, Input : String;
      Contents        : out Preprocessed_Source;
      Diagnostics     : in out Diagnostics_Vectors.Vector);
   --  Preprocess the ``Input`` source buffer according to the corresponding
   --  source filename ``Filename`` and the given preprocessor data.
   --
   --  Note that ``Filename`` is used here only to look in ``Data`` for the
   --  ``File_Config`` value to use in order to preprocess the ``Input`` source
   --  buffer.
   --
   --  On success, leave ``Diagnostics`` empty and return in ``Contents`` a
   --  newly allocated string containing the preprocessed source.
   --
   --  On failure, leave ``Contents`` uninitialized and put error messages in
   --  ``Diagnostics``.

   ------------------
   -- File readers --
   ------------------

   function Create_Preprocessor
     (Default_Config : in out File_Config;
      File_Configs   : in out File_Config_Maps.Map)
      return File_Reader_Reference;
   --  Like ``Create_Preprocessor_Data``, but return a file reader implementing
   --  the preprocessing instead.

   function Create_Preprocessor_From_File
     (Filename : String; Path : Any_Path) return File_Reader_Reference;
   --  Like ``Parse_Preprocessor_Data_File``, but return a file reader
   --  implementing the preprocessing instead.

   function Create_Preprocessor_From_File
     (Filename  : String;
      Path      : Any_Path;
      Line_Mode : Any_Line_Mode) return File_Reader_Reference;
   --  Convenience ``Create_Preprocessor_From_File`` overload, to force a given
   --  line mode for all source files on which the preprocessor is enabled.
   --
   --  Forcing the line mode is often needed as the default is to remove
   --  lines that contain preprocessor directives and disabled code, which
   --  breaks the line number correspondance between original source code and
   --  preprocessed one. Forcing to ``Blank_Lines`` or ``Comment_Lines``
   --  preserves this correspondance.

   -------------------
   -- Debug helpers --
   -------------------

   procedure Dump (Definitions : Definition_Maps.Map; Prefix : String := "");
   --  Dump the content of ``Definitions`` on the standard output, each line
   --  prefixed with ``Prefix``.

   procedure Dump (Config : File_Config; Prefix : String := "");
   --  Dump the content of ``Config`` on the standard output, each line
   --  prefixed with ``Prefix``.

   procedure Dump (Data : Preprocessor_Data; Prefix : String := "");
   --  Dump the content of ``Data`` on the standard output, each line prefixed
   --  with ``Prefix``.

private
   use Libadalang.Analysis;

   --  We want ``Preprocessor_Data`` to be a reference to constant data:
   --  implement it as a shared pointer.

   type Preprocessor_Data_Record is record
      Ref_Count : Natural;
      --  Number of ``Preprocessor_Data`` objects that refer to this record

      Default_Config : File_Config;
      --  Preprocessor configuration to use for Ada source files not present in
      --  the ``Files_Configs`` map.

      File_Configs : File_Config_Maps.Map;
      --  For each Ada source file in this map, preprocessor configuration to
      --  use (other sources must use ``Default_Config``).

      Context : Analysis_Context;
      --  Context used to parse preprocessing directives. Allocated once for
      --  each ``Preprocessor_Data_Record`` object, for efficiency.
   end record;
   type Preprocessor_Data_Access is access all Preprocessor_Data_Record;

   type Preprocessor_Data is new Ada.Finalization.Controlled with record
      Data : Preprocessor_Data_Access;
   end record;

   overriding procedure Adjust (Self : in out Preprocessor_Data);
   overriding procedure Finalize (Self : in out Preprocessor_Data);

   No_Preprocessor_Data : constant Preprocessor_Data :=
     (Ada.Finalization.Controlled with Data => null);

end Libadalang.Preprocessing;
