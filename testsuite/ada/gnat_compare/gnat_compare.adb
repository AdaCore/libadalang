with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.OS;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with GPR2.Build.Source;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.Common;     use Libadalang.Common;
with Libadalang.Helpers;    use Libadalang.Helpers;

with String_Utils; use String_Utils;
with Xrefs;        use Xrefs;
with Xrefs_Wrapper;

procedure GNAT_Compare is

   type Comparison_Type is
     (Ok,
      --  When the xref from GNAT is the same as the xref from LAL

      Different,
      --  When both refs are equal but the referenced entity is not

      Error,
      --  When LAL raises a Property_Error

      Missing,
      --  When a xref from GNAT is missing from LAL

      Additional
      --  When a xref from LAL is missing from GNAT
     );
   --  Kind for differences between xrefs in GNAT and xrefs in LAL

   type Comparison_Set is array (Comparison_Type) of Boolean;

   function Convert (Arg : String) return Comparison_Set;

   procedure Job_Setup (Context : App_Job_Context);
   --  Import command line arguments to our global state and load xrefs from
   --  the Library Files in Project.

   procedure Job_Post_Process (Context : App_Job_Context);

   package App is new Libadalang.Helpers.App
     (Name             => "gnat_compare",
      Description      => "Compare GNAT's xrefs and Libadalang's",
      Job_Setup        => Job_Setup,
      Job_Post_Process => Job_Post_Process);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Comparisons is new Parse_Option_List
        (App.Args.Parser, "-d", "--comparisons",
         "Select what differences between GNAT's xrefs and Libadalang's to"
         & " report",
         Accumulate => True,
         Arg_Type   => Comparison_Set);

      package Show_Nodes is new Parse_Flag
        (App.Args.Parser, "-n", "--show-nodes",
         "Print the declarations to which Libadalang resolved");

      package Ignore_Columns is new Parse_Flag
        (App.Args.Parser, "-c", "--ignore-columns",
         "Ignore differences in column numbers");

      package Skip_Build is new Parse_Flag
        (App.Args.Parser, "-b", "--skip-build",
         "Skip the build of the project to process");

      package Skip_GPRclean is new Parse_Flag
        (App.Args.Parser, "-sc", "--skip-gprclean",
         "Skip the run of gprclean at the end of gnat_compare execution");
   end Args;

   Enabled : Comparison_Set := [others => True];
   --  For each kind of xrefs difference, determine whether we should report it

   Count_Enabled : Comparison_Set := [others => True];
   --  For each kind of xrefs difference, say whether we should compute stats
   --  about it.

   GNAT_Xref_Count : Natural := 0;
   --  Number of Xrefs entries in GNAT (total)

   Counts : array (Comparison_Type) of Natural := [others => 0];
   --  For each kind of xrefs difference, hit count

   Show_Nodes     : Boolean := False;
   --  In report, whether to show nodes that LAL uses to resolve references

   Ignore_Columns : Boolean := False;
   --  Whether to ignore differences in column numbers for referenced entities

   Source_Files : String_Vectors.Vector;
   Files        : File_Table_Type;
   LI_Xrefs     : Unit_Xrefs_Vectors.Vector;

   procedure Report
     (Files               : File_Table_Type;
      GNAT_Xref, LAL_Xref : Xref_Type;
      Comp                : Comparison_Type;
      LAL_Node            : Ada_Node'Class);
   --  Depending on the Enabled array, emit a diagnostic for the comparison
   --  between GNAT_Xref and LAL_Xref, whose kind is Comp.

   procedure Run_GPRbuild (Project_File : String);
   --  Run "gprbuild" on Project_File

   procedure Run_GPRclean (Project_File : String);
   --  Run "gprclean" on Project_File

   procedure Load_All_Xrefs_From_LI
     (Tree         : GPR2.Project.Tree.Object;
      Files        : in out File_Table_Type;
      Xrefs        : out Unit_Xrefs_Vectors.Vector;
      Source_Files : String_Vectors.Vector);
   --  Go through all library files in Tree and read the xref information they
   --  contain. Build the Xrefs database from it.
   --
   --  Register only xrefs for references that come from files in Source_Files.
   --  Use all references if Source_Files is empty.

   procedure Compare_Xrefs
     (Files : in out File_Table_Type;
      Root  : Ada_Node;
      Xrefs : Xref_Vectors.Vector);
   --  Go through all files referenced in the Xrefs database and use LAL to
   --  resolve all xrefs. Compare both, reporting the differences using the
   --  Report procedure above.

   function Get_Project
     (Context : App_Job_Context) return GPR2.Project.Tree.Object;
   --  If a project file was loaded, return it. Return ``Undefined`` otherwise

   -------------
   -- Convert --
   -------------

   function Convert (Arg : String) return Comparison_Set is
   begin
      return Result : Comparison_Set := [others => True] do
         for C of Arg loop
            declare
               Comp : constant Comparison_Type :=
                 (case C is
                  when 'o' => Ok,
                  when 'd' => Different,
                  when 'e' => Error,
                  when 'm' => Missing,
                  when 'a' => Additional,
                  when others => raise Constraint_Error
                                 with "Invalid comparison: " & C);
            begin
               Result (Comp) := False;
            end;
         end loop;
      end return;
   end Convert;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Context : App_Job_Context) return GPR2.Project.Tree.Object
   is
      Provider : Source_Provider renames Context.App_Ctx.Provider;
   begin
      return (if Provider.Kind = Project_File
              then Provider.Project
              else GPR2.Project.Tree.Undefined);
   end Get_Project;

   ---------------
   -- Job_Setup --
   ---------------

   procedure Job_Setup (Context : App_Job_Context) is
      Tree         : constant GPR2.Project.Tree.Object :=
        Get_Project (Context);
      Project_File : constant String := To_String (App.Args.Project_File.Get);
   begin
      if not Tree.Is_Defined then
         Abort_App ("Please provide a project file (-P/--project).");
      end if;

      Show_Nodes := Args.Show_Nodes.Get;
      Ignore_Columns := Args.Ignore_Columns.Get;

      for Comparison_Set of Args.Comparisons.Get loop
         for Comp in Comparison_Set'Range loop
            if Comparison_Set (Comp) then
               Enabled (Comp) := False;

               Count_Enabled (Comp) := Comp = Ok;
               --  Count OK xrefs even when they are not displayed in the
               --  report.  This is less surprising.
            end if;
         end loop;
      end loop;

      --  Build the input project (if requested) and import the resulting xrefs
      --  database.

      if Project_File /= "" and then not Args.Skip_Build.Get then
         Run_GPRbuild (Project_File);
      end if;
      Load_All_Xrefs_From_LI (Tree, Files, LI_Xrefs, Source_Files);
   end Job_Setup;

   ----------------------
   -- Job_Post_Process --
   ----------------------

   procedure Job_Post_Process (Context : App_Job_Context) is
      View : constant GPR2.Project.View.Object :=
        Get_Project (Context).Root_Project;
   begin
      --  Browse this database and compare it to what LAL can resolve

      Sort (Files, LI_Xrefs);
      for Unit_Xrefs of LI_Xrefs loop
         declare
            Name   : constant String := Filename (Files, Unit_Xrefs.Unit);
            Source : constant GPR2.Build.Source.Object :=
              View.Visible_Source (GPR2.Simple_Name (Name));
            Unit   : Analysis_Unit;
         begin
            Put_Line ("== " & Name & " ==");

            if Source.Is_Defined then
               Unit := Context.Analysis_Ctx.Get_From_File
                         (String (Source.Path_Name.Value));

               if Unit.Has_Diagnostics then
                  for D of Unit.Diagnostics loop
                     Put_Line (Standard_Error, Unit.Format_GNU_Diagnostic (D));
                  end loop;
                  Abort_App;
               end if;

               Sort (Files, Unit_Xrefs.Xrefs);
               Remove_Duplicates (Unit_Xrefs.Xrefs);

               GNAT_Xref_Count :=
                 GNAT_Xref_Count + Natural (Unit_Xrefs.Xrefs.Length);

               Compare_Xrefs (Files, Root (Unit), Unit_Xrefs.Xrefs);
            else
               Put_Line ("warning: could not locate source file: " & Name);
               Put_Line ("warning: discarding results for it");
            end if;
            Free (Unit_Xrefs);
         end;
      end loop;

      New_Line;
      if GNAT_Xref_Count = 0 then
         Put_Line ("No stats");
      else
         Put_Line ("Stats:");
         Put_Line
           ("GNAT xrefs have" & Natural'Image (GNAT_Xref_Count) & " entries");
         Put_Line ("LAL xrefs have:");
         for Comp in Comparison_Type'Range loop
            if Count_Enabled (Comp) then
               declare
                  type Percentage is delta 0.01 range 0.0 .. 0.01 * 2.0**32;

                  Count : constant Natural := Counts (Comp);
                  P     : constant Float :=
                    100.0 * Float (Count) / Float (GNAT_Xref_Count);
                  P_Img : constant String := Percentage'Image (Percentage (P));
               begin
                  Put_Line
                    ("  *" & Natural'Image (Count)
                     & " " & Comparison_Type'Image (Comp) & " entries ("
                     & P_Img (P_Img'First + 1 .. P_Img'Last) & "%)");
               end;
            end if;
         end loop;
      end if;

      if not Args.Skip_GPRclean.Get then
         declare
            Project_File : constant String :=
               To_String (App.Args.Project_File.Get);
         begin
            Run_GPRclean (Project_File);
         end;
      end if;
   end Job_Post_Process;

   ------------------
   -- Run_GPRbuild --
   ------------------

   procedure Run_GPRbuild (Project_File : String) is
      Args    : GNATCOLL.OS.Process.Argument_List;
      Success : Integer;
   begin
      Args.Append ("gprbuild");
      Args.Append (+"-c");
      Args.Append (+"-q");
      Args.Append (+"-p");
      Args.Append (+"-P" & Project_File);
      for V of App.Args.Scenario_Vars.Get loop
         Args.Append ("-X" & To_String (V));
      end loop;

      Success := GNATCOLL.OS.Process.Run
        (Args, "", FS.Standin, FS.Standerr, FS.Null_FD, INHERIT);

      if Success /= 0 then
         Abort_App ("Could not spawn gprbuild");
      end if;
   end Run_GPRbuild;

   ------------------
   -- Run_GPRbuild --
   ------------------

   procedure Run_GPRclean (Project_File : String) is
      Args    : GNATCOLL.OS.Process.Argument_List;
      Success : Integer;
   begin
      Args.Append ("gprclean");
      Args.Append (+"-q");
      Args.Append (+"-r");
      Args.Append (+"-P" & Project_File);
      for V of App.Args.Scenario_Vars.Get loop
         Args.Append ("-X" & To_String (V));
      end loop;

      Success := GNATCOLL.OS.Process.Run
        (Args, "", FS.Standin, FS.Standerr, FS.Null_FD, INHERIT);

      if Success /= 0 then
         Abort_App ("Could not spawn gprclean");
      end if;
   end Run_GPRclean;

   ----------------------------
   -- Load_All_Xrefs_From_LI --
   ----------------------------

   procedure Load_All_Xrefs_From_LI
     (Tree         : GPR2.Project.Tree.Object;
      Files        : in out File_Table_Type;
      Xrefs        : out Unit_Xrefs_Vectors.Vector;
      Source_Files : String_Vectors.Vector)
   is
      View : constant GPR2.Project.View.Object := Tree.Root_Project;
   begin
      if View.Kind not in GPR2.With_Object_Dir_Kind then
         return;
      end if;

      declare
         Filenames : File_Array_Access :=
           View.Object_Directory.Virtual_File.Read_Dir (Files_Only);
         New_Xrefs : Unit_Xrefs_Vectors.Vector;
      begin
         for F of Filenames.all loop
            if F.File_Extension = ".ali" then
               New_Xrefs.Clear;
               Read_LI_Xrefs (+F.Full_Name, Files, New_Xrefs);
               for NX of New_Xrefs loop
                  if Source_Files.Is_Empty
                    or else Source_Files.Contains (+Filename (Files, NX.Unit))
                  then
                     Xrefs.Append (NX);
                  else
                     Free (NX);
                  end if;
               end loop;
            end if;
         end loop;
         Unchecked_Free (Filenames);
      end;
   end Load_All_Xrefs_From_LI;

   -------------------
   -- Compare_Xrefs --
   -------------------

   procedure Compare_Xrefs
     (Files : in out File_Table_Type;
      Root  : Ada_Node;
      Xrefs : Xref_Vectors.Vector)
   is

      Index  : constant File_Index_Type :=
        File_Index (Files, Get_Filename (Root.Unit));

      Cursor : Natural := Xrefs.First_Index;
      --  Index of the next xref in Xrefs to use for comparison

      function Traverse (Node : Ada_Node'Class) return Visit_Status;
      --  Called for all AST nodes under Root

      function Resolve
        (Node : Ada_Node; Error : out Boolean) return Defining_Name;
      --  Try to resolve Node into the corresponding declaration, applying
      --  post-processing from Xrefs_Wrapper.

      procedure Process (LAL_Xref : Xref_Type; LAL_Node : Ada_Node'Class);
      --  Helper called from Traverse to run for all resolutions that either
      --  failed or succeeded and returned a non-null referenced declaration.

      --------------
      -- Traverse --
      --------------

      function Traverse (Node : Ada_Node'Class) return Visit_Status is
         Ref  : Defining_Name;
         Rel  : Libadalang.Analysis.Name;
         Xref : Xref_Type;
      begin
         --  GNAT only considers leaf items for xrefs, so skip for instance
         --  Dotted_Name nodes here.
         if Node.Kind not in Ada_String_Literal | Ada_Identifier | Ada_Op then
            return Into;
         end if;

         --  Node is the "referencing" part of the xref...
         Xref.Ref_Sloc := Start_Sloc (Node.Sloc_Range);
         Xref.Ref_File := Index;
         Xref.Error := False;

         --  String literal names have slocs after the "
         if Node.Kind = Ada_String_Literal then
            Xref.Ref_Sloc.Column  := Xref.Ref_Sloc.Column + 1;
         end if;

         --  ... Ref will be the "referenced" part.
         begin
            Ref := Resolve (Node.As_Ada_Node, Xref.Error);
         end;

         if Xref.Error then
            null;

         elsif not Ref.Is_Null then

            --  Take the relative name because that's what GNAT does
            Rel := Ref.P_Relative_Name;

            Xref.Entity_Sloc := Start_Sloc (Rel.Sloc_Range);

            --  When an entity whose name is a string literal is referenced,
            --  GNAT puts the column after the first ". Let's mimic that
            --  behavior.

            if Rel.Kind = Ada_String_Literal then
               Xref.Entity_Sloc.Column := Xref.Entity_Sloc.Column + 1;
            end if;

            Xref.Entity_File :=
              File_Index (Files, Get_Filename (Ref.Unit));

         else
            --  When execution reached this place, we got no error and the
            --  referenced entity is "null", which means: this resolves to
            --  nothing. So consider there is no xref.
            return Into;
         end if;

         Process (Xref, Node);
         return Into;
      end Traverse;

      -------------
      -- Resolve --
      -------------

      function Resolve
        (Node : Ada_Node; Error : out Boolean) return Defining_Name
      is
         Ref : Defining_Name;
      begin
         Error := False;

         begin
            Ref := Node.P_Gnat_Xref;
         exception
            when Property_Error =>
               Error := True;
         end;

         if not Ref.Is_Null then
            for Wrapper of Xrefs_Wrapper.Post_Wrappers loop
               declare
                  Wrapped_Ref : constant Defining_Name := Wrapper (Node, Ref);
               begin
                  if not Wrapped_Ref.Is_Null then
                     return Wrapped_Ref;
                  end if;
               end;
            end loop;
         end if;

         return Ref;
      end Resolve;

      -------------
      -- Process --
      -------------

      procedure Process (LAL_Xref : Xref_Type; LAL_Node : Ada_Node'Class) is
      begin
         while Cursor <= Xrefs.Last_Index loop
            declare
               GNAT_Xref : constant Xref_Type := Xrefs (Cursor);
               Comp      : Comparison_Type;
            begin
               pragma Assert (LAL_Xref.Ref_File = GNAT_Xref.Ref_File);

               --  Go through all entries in Xrefs that appear in the source
               --  file before the "referencing" part in LAL_Xref...

               case Compare (GNAT_Xref.Ref_Sloc, LAL_Xref.Ref_Sloc) is
                  when After =>
                     --  Here, GNAT_Xref appears before LAL_Xref, so LAL failed
                     --  to resolve it.

                     Report (Files, GNAT_Xref, LAL_Xref, Missing, LAL_Node);
                     Cursor := Cursor + 1;

                  when Inside =>
                     --  GNAT_Xref and LAL_Xref have the same "referencing"
                     --  part: consider they are both resolving the same
                     --  reference. Check that they both reference to the
                     --  same declaration (ignoring column number if asked to).

                     if LAL_Xref.Error then
                        Comp := Error;

                     elsif GNAT_Xref.Entity_Sloc = LAL_Xref.Entity_Sloc
                       or else (Ignore_Columns
                                and then GNAT_Xref.Entity_Sloc.Line
                                = LAL_Xref.Entity_Sloc.Line)
                     then
                        Comp := Ok;

                     else
                        Comp := Different;
                     end if;

                     Report (Files, GNAT_Xref, LAL_Xref, Comp, LAL_Node);
                     Cursor := Cursor + 1;
                     exit;

                  when Before =>
                     exit;
               end case;
            end;
         end loop;
      end Process;

   begin
      Root.Traverse (Traverse'Access);

      --  Here, we tried to resolve all nodes under Root, so if we still have
      --  unprocessed xrefs from GNAT, report them as missing from LAL.

      while Cursor <= Xrefs.Last_Index loop
         Report (Files, Xrefs (Cursor), No_Xref, Missing, No_Ada_Node);
         Cursor := Cursor + 1;
      end loop;
   end Compare_Xrefs;

   ------------
   -- Report --
   ------------

   procedure Report
     (Files               : File_Table_Type;
      GNAT_Xref, LAL_Xref : Xref_Type;
      Comp                : Comparison_Type;
      LAL_Node            : Ada_Node'Class) is
   begin
      if Count_Enabled (Comp) then
         Counts (Comp) := Counts (Comp) + 1;
      end if;

      if not Enabled (Comp) then
         return;
      end if;

      case Comp is
         when Ok | Different | Error | Missing =>
            Put (Files, GNAT_Xref);
            if Comp = Different then
               declare
                  F    : constant String :=
                    Filename (Files, LAL_Xref.Entity_File);
                  Sloc : constant String :=
                    (if F = "__standard"
                     then "in Standard"
                     else F & ":" & Image (LAL_Xref.Entity_Sloc));
               begin
                  Put (" (LAL: " & Sloc & ')');
               end;
            elsif Comp = Missing then
               Put (" (LAL: missing)");
            elsif Comp = Error then
               Put (" (LAL: error)");
            else
               Put (" (LAL: ok)");
            end if;

         when Additional =>
            Put (Files, LAL_Xref);
            Put (" (GNAT: missing)");
      end case;

      if Show_Nodes and then not LAL_Node.Is_Null then
         Put (' ' & LAL_Node.Image);
      end if;
      New_Line;
   end Report;

begin
   App.Run;
end GNAT_Compare;
