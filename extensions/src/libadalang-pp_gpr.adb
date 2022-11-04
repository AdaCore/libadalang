--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.Utils;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Source;

package body Libadalang.PP_GPR is

   -------------
   -- No_View --
   -------------

   function No_View (Tree : Any_Tree) return Any_View is
   begin
      case Tree.Kind is
      when GPR1_Kind =>
         return (Kind => GPR1_Kind, GPR1_Value => GPR1.No_Project);
      when GPR2_Kind =>
         return (Kind => GPR2_Kind, GPR2_Value => GPR2.Project.View.Undefined);
      end case;
   end No_View;

   ----------
   -- Root --
   ----------

   function Root (Self : Any_Tree) return Any_View is
   begin
      case Self.Kind is
      when GPR1_Kind =>
         return (Kind       => GPR1_Kind,
                 GPR1_Value => Self.GPR1_Value.Root_Project);
      when GPR2_Kind =>
         return (Kind       => GPR2_Kind,
                 GPR2_Value => Self.GPR2_Value.Root_Project);
      end case;
   end Root;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Self : Any_View; Process : access procedure (Self : Any_View)) is
   begin
      case Self.Kind is
      when GPR1_Kind =>
         declare
            use type GPR1.Project_Type;

            It : GPR1.Project_Iterator := Self.GPR1_Value.Start;
            P  : GPR1.Project_Type;
         begin
            loop
               P := GPR1.Current (It);
               exit when P = GPR1.No_Project;
               Process.all ((Kind => GPR1_Kind, GPR1_Value => P));
               GPR1.Next (It);
            end loop;
         end;

      when GPR2_Kind =>

         --  ``View`` is not in its own closure, so process it first

         Process.all (Self);

         --  If ``View`` is an aggregate project, also go through the
         --  aggregated projects.

         if Self.GPR2_Value.Kind in GPR2.Aggregate_Kind then
            for P of Self.GPR2_Value.Aggregated loop
               Iterate ((Kind => GPR2_Kind, GPR2_Value => P), Process);
            end loop;
         end if;

         --  If ``View`` extends another project, also go through that other
         --  project.
         --
         --  TODO (VB04-038)??? This may cause ``Iterate`` to process the same
         --  project multiple times: it is inefficient, but should not be a
         --  problem. Hopefully at some point GPR2 will provide an exhaustive
         --  iteration scheme which we will be able to use here as well.

         if Self.GPR2_Value.Is_Extending then
            Iterate
              ((Kind       => GPR2_Kind,
                GPR2_Value => Self.GPR2_Value.Extended_Root), Process);
         end if;

         for View of Self.GPR2_Value.Closure loop
            Process.all ((Kind => GPR2_Kind, GPR2_Value => View));
         end loop;
      end case;
   end Iterate;

   ----------------
   -- Object_Dir --
   ----------------

   function Object_Dir (Self : Any_View) return String is
   begin
      case Self.Kind is
      when GPR1_Kind =>
         return +Self.GPR1_Value.Object_Dir.Full_Name;
      when GPR2_Kind =>
         return String (Self.GPR2_Value.Object_Directory.Value);
      end case;
   end Object_Dir;

   -------------
   -- Indexes --
   -------------

   function Indexes
     (Self : Any_View; Attribute : Any_Attribute) return XString_Array is
   begin
      case Self.Kind is
      when GPR1_Kind =>
         declare
            Indexes : String_List :=
              Self.GPR1_Value.Attribute_Indexes (Attribute.GPR1_Value.all);
         begin
            return Result : XString_Array (Indexes'Range) do
               for I in Result'Range loop
                  Result (I) := To_XString (Indexes (I).all);
               end loop;
               GNATCOLL.Utils.Free (Indexes);
            end return;
         end;

      when GPR2_Kind =>
         declare
            Attrs : constant GPR2.Project.Attribute.Set.Object :=
              Self.GPR2_Value.Attributes (Attribute.GPR2_Value);
            I     : Positive := 1;
         begin
            return Result : XString_Array (1 .. Natural (Attrs.Length)) do
               for A of Attrs loop
                  Result (I) := To_XString (A.Index.Text);
                  I := I + 1;
               end loop;
            end return;
         end;
      end case;
   end Indexes;

   ------------
   -- Values --
   ------------

   function Values
     (Self      : Any_View;
      Attribute : Any_Attribute;
      Index     : String) return XString_Array is
   begin
      case Self.Kind is
      when GPR1_Kind =>
         declare
            Values : String_List_Access :=
              Self.GPR1_Value.Attribute_Value
                (Attribute.GPR1_Value.all, Index);
         begin
            if Values = null then
               return (1 .. 0 => <>);
            else
               return Result : XString_Array (Values.all'Range) do
                  for I in Result'Range loop
                     Result (I) := To_XString (Values.all (I).all);
                  end loop;
                  Free (Values);
               end return;
            end if;
         end;

      when GPR2_Kind =>
         declare
            Attr : constant GPR2.Project.Attribute.Object :=
              Self.GPR2_Value.Attribute
                (Name  => Attribute.GPR2_Value,
                 Index => GPR2.Project.Attribute_Index.Create (Index));
            Values : constant GPR2.Containers.Source_Value_List :=
              (if Attr.Is_Defined
               then Attr.Values
               else GPR2.Containers.Source_Value_Type_List.Empty_Vector);
         begin
            return Result : XString_Array (1 .. Natural (Values.Length)) do
               for I in Result'Range loop
                  Result (I) := To_XString (Values (I).Text);
               end loop;
            end return;
         end;
      end case;
   end Values;

   -------------------
   -- Is_Ada_Source --
   -------------------

   function Is_Ada_Source
     (Tree : Any_Tree; View : Any_View; Filename : String) return Boolean is
   begin
      case Tree.Kind is
      when GPR1_Kind =>
         declare
            use type GPR1.Project_Type;

            Path  : constant GNATCOLL.VFS.Virtual_File := Create (+Filename);
            Infos : constant GPR1.File_Info_Set :=
              Tree.GPR1_Value.Info_Set (Path);
         begin
            return
              (for some Info of Infos =>
               GPR1.File_Info (Info).Project (Root_If_Not_Found => False)
                 = View.GPR1_Value
               and then To_Lower (GPR1.File_Info (Info).Language) = "ada");
         end;

      when GPR2_Kind =>
         declare
            Path : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_File
                (GPR2.Filename_Type (Filename), GPR2.Path_Name.No_Resolution);
            File : constant GPR2.Project.Source.Object :=
              View.GPR2_Value.Source (Path);
         begin
            return File.Is_Defined and then File.Is_Ada;
         end;
      end case;
   end Is_Ada_Source;

   --------------------------------------------
   -- Extract_Preprocessor_Data_From_Project --
   --------------------------------------------

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Any_Tree;
      View           : Any_View;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map)
   is
      Root_Project : constant Any_View :=
        (if View = No_View (Tree)
         then Root (Tree)
         else View);
      --  Subproject from which to start probing compilation options

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
        (P : Any_View; Attribute : Any_Attribute; Index : String);
      --  Add the command-line arguments in the ``Attribute (Index)`` project
      --  attribute in ``P`` to our knowledge base: the ``Default_Config`` and
      --  ``File_Configs`` arguments (for preprocesor data files) and ``Defs``
      --  (for symbol definitions).

      procedure Process_View (Self : Any_View);
      --  Process both default switches for all Ada sources, then unit-specific
      --  switches in the ``Self`` project.

      ----------------------
      -- Process_Switches --
      ----------------------

      procedure Process_Switches
        (P : Any_View; Attribute : Any_Attribute; Index : String)
      is
         --  Prefixes for the command-line options to match

         Def_Prefix  : constant String := "-gnateD";
         File_Prefix : constant String := "-gnatep=";
      begin
         for Arg of Values (P, Attribute, Index) loop

            --  If this option defines a symbol, add it to our list of symbols

            if Arg.Starts_With (Def_Prefix) then
               declare
                  Option : constant XString :=
                    Arg.Slice (Def_Prefix'Length + 1, Arg.Length);
                  Name   : US.Unbounded_String;
                  Value  : Value_Type;
               begin
                  Parse_Definition_Option (Option.To_String, Name, Value);
                  Defs.Include (Name, Value);
               end;

            --  If this is the first option we see that uses a preprocessor
            --  data file, load it.

            elsif Arg.Starts_With (File_Prefix)
                  and then not Prep_Data_File_Found
            then
               declare
                  File : constant XString :=
                    Arg.Slice (File_Prefix'Length + 1, Arg.Length);
                  --  Name of the preprocessor data file. It may appear
                  --  absolute or relative in the project file.

                  Path : constant Any_Path := Create_Path
                    (Directories => (1 => To_XString (Object_Dir (P))),
                     CWD         => If_Empty);
                  --  If the proprocesor data file is not absolute, it is
                  --  relative to the object directory.
               begin
                  Parse_Preprocessor_Data_File
                    (File.To_String, Path, Default_Config, File_Configs);
               end;
               Prep_Data_File_Found := True;
            end if;
         end loop;
      end Process_Switches;

      ------------------
      -- Process_View --
      ------------------

      procedure Process_View (Self : Any_View) is
         Kind             : Project_Kind renames Self.Kind;
         Default_Switches : Any_Attribute renames
           Attributes.Default_Switches (Kind);
         Switches         : Any_Attribute renames Attributes.Switches (Kind);
      begin
         --  Process default compiler switches for the Ada language

         Process_Switches (Self, Default_Switches, "Ada");

         --  Also process compiler switches for all Ada sources

         for Source of Indexes (Self, Switches) loop
            declare
               Filename : constant String := Source.To_String;
            begin
               if Is_Ada_Source (Tree, Self, Filename) then
                  Process_Switches (Self, Switches, Filename);
               end if;
            end;
         end loop;
      end Process_View;

   begin
      Default_Config := Disabled_File_Config;
      File_Configs := File_Config_Maps.Empty_Map;

      --  Go through all subprojects and extract preprocessor data from their
      --  compiler switches.

      Iterate (Root_Project, Process_View'Access);

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
