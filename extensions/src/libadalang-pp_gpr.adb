--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GNATCOLL.Strings;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with GNATCOLL.Utils;      use GNATCOLL.Utils;

with Libadalang.Common;   use Libadalang.Common;

package body Libadalang.PP_GPR is

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Prj.Project_Tree'Class;
      Project        : Prj.Project_Type := Prj.No_Project;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map)
   is
      Root_Project : constant Prj.Project_Type :=
        (if Project = No_Project
         then Tree.Root_Project
         else Project);
      --  Subproject from which to start probing compilation options

      function Starts_With (Str, Substr : String) return Boolean is
        (Str'Length >= Substr'Length
         and then Str (Str'First .. Str'First + Substr'Length - 1) = Substr);
      --  Return whether ``Str`` starts with the ``Substr`` substring

      Defs : Definition_Maps.Map;
      --  Keep track of the definitions

      Prep_Data_File_Found : Boolean := False;
      --  Whether we have found a preprocessor data file

      --  Loading preprocessor data is done in two steps: first load the
      --  preprocessor data file (if any), then use the symbol definitions
      --  found as compilation options. This matches what GNAT implements:
      --  ``-gnateDX=Y -gnatep=foo.txt`` sets the ``X`` symbol to ``Y`` even if
      --  ``foo.txt`` contains another symbol definition for ``X``.

      procedure Process_Switches
        (P : Project_Type; Attribute : Attribute_Pkg_List; Index : String);
      --  Add the command-line arguments in the ``Attribute (Index)`` project
      --  attribute in ``P`` to our knowledge base: the ``Default_Config`` and
      --  ``File_Configs`` arguments (for preprocesor data files) and ``Defs``
      --  (for symbol definitions).

      ----------------------
      -- Process_Switches --
      ----------------------

      procedure Process_Switches
        (P : Project_Type; Attribute : Attribute_Pkg_List; Index : String)
      is
         --  Prefixes for the command-line options to match

         Def_Prefix  : constant String := "-gnateD";
         File_Prefix : constant String := "-gnatep=";

         Switches : String_List_Access := P.Attribute_Value (Attribute, Index);
      begin
         if Switches = null then
            return;
         end if;

         for Arg of Switches.all loop

            --  If this option defines a symbol, add it to our list of symbols

            if Starts_With (Arg.all, Def_Prefix) then
               declare
                  Option : String renames Arg.all
                    (Arg.all'First + Def_Prefix'Length .. Arg.all'Last);
                  Name   : US.Unbounded_String;
                  Value  : Value_Type;
               begin
                  Parse_Definition_Option (Option, Name, Value);
                  Defs.Include (Name, Value);
               end;

            --  If this is the first option we see that uses a preprocessor
            --  data file, load it.

            elsif Starts_With (Arg.all, File_Prefix)
                  and then not Prep_Data_File_Found
            then
               declare
                  use GNATCOLL.Strings;

                  File : String renames Arg.all
                    (Arg.all'First + File_Prefix'Length .. Arg.all'Last);
                  --  Name of the preprocessor data file. It may appear
                  --  absolute or relative in the project file.

                  Path : constant Any_Path := Create_Path
                    (Directories => (1 => To_XString
                                            (+P.Object_Dir.Full_Name)),
                     CWD         => If_Empty);
                  --  If the proprocesor data file is not absolute, it is
                  --  relative to the object directory.
               begin
                  Parse_Preprocessor_Data_File
                    (File, Path, Default_Config, File_Configs);
               end;
               Prep_Data_File_Found := True;
            end if;
         end loop;
         Free (Switches);

      exception
         when File_Read_Error | Syntax_Error =>
            Free (Switches);
            raise;
      end Process_Switches;

      It : Project_Iterator := Root_Project.Start;
      P  : Project_Type;
   begin
      Default_Config := Disabled_File_Config;
      File_Configs := File_Config_Maps.Empty_Map;

      loop
         P := Current (It);
         exit when P = No_Project;

         --  Process both default switches for all Ada sources, then
         --  unit-specific switches.

         Process_Switches (P, Compiler_Default_Switches_Attribute, "Ada");
         declare
            Attr    : constant Attribute_Pkg_List :=
              Build ("Compiler", "Switches");
            Sources : String_List := P.Attribute_Indexes (Attr);
         begin
            for S of Sources loop
               declare
                  File  : constant GNATCOLL.VFS.Virtual_File :=
                    Create (+S.all);
                  Infos : constant File_Info_Set := Tree.Info_Set (File);
               begin
                  if (for some Info of Infos =>
                      File_Info (Info).Project (Root_If_Not_Found => False) = P
                      and then To_Lower (File_Info (Info).Language) = "ada")
                  then
                     Process_Switches (P, Attr, S.all);
                  end if;
               end;
            end loop;
            Free (Sources);

         exception
            when File_Read_Error | Syntax_Error =>
               Free (Sources);
               raise;
         end;

         Next (It);
      end loop;

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
                  Config := (Enabled => True, others => <>);
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

end Libadalang.PP_GPR;
