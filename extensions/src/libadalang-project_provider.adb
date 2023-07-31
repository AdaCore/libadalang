--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

with GNAT.OS_Lib;
with GNAT.Task_Lock;

with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.VFS;     use GNATCOLL.VFS;
with GPR2.Project.Unit_Info;
with GPR2.Unit;

with Libadalang.GPR_Impl;
with Libadalang.GPR_Utils;         use Libadalang.GPR_Utils;
with Libadalang.Implementation;    use Libadalang.Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Unit_Files;

package body Libadalang.Project_Provider is

   package Filename_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Virtual_File,
      Hash                => Full_Name_Hash,
      Equivalent_Elements => "=");

   use type Ada.Containers.Count_Type;

   package US renames Ada.Strings.Unbounded;
   use type US.Unbounded_String;

   ------------------------
   -- GPR1 unit provider --
   ------------------------

   type Project_Data (Kind : Project_Kind := GPR1_Kind) is record
      case Kind is
         when GPR1_Kind =>
            GPR1_Tree             : Prj.Project_Tree_Access;
            GPR1_Env              : Prj.Project_Environment_Access;
            GPR1_Is_Project_Owner : Boolean;
         when GPR2_Kind =>
            GPR2_Tree : access GPR2.Project.Tree.Object;
      end case;
   end record;

   type Project_Unit_Provider is new LAL.Unit_Provider_Interface with record
      Data     : Project_Data;
      Projects : View_Vectors.Vector;
   end record;
   --  Unit provider backed up by a project file

   type Project_Unit_Provider_Access is access all Project_Unit_Provider;

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding procedure Get_Unit_Location
     (Provider       : Project_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out US.Unbounded_String;
      PLE_Root_Index : in out Natural);

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class;

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Project_Unit_Provider;
      Context        : LAL.Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out LAL.Analysis_Unit'Class;
      PLE_Root_Index : in out Natural);

   overriding procedure Release (Provider : in out Project_Unit_Provider);

   ------------------------------------------
   -- Helpers to create project partitions --
   ------------------------------------------

   type Any_Provider_And_Projects is record
      Provider : LAL.Unit_Provider_Reference;
      Projects : View_Vectors.Vector;
   end record;
   --  Provider_And_Projects equivalent that is GPR library agnostic

   type Any_Provider_And_Projects_Array is
     array (Positive range <>) of Any_Provider_And_Projects;
   type Any_Provider_And_Projects_Array_Access is
      access all Any_Provider_And_Projects_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Any_Provider_And_Projects_Array, Any_Provider_And_Projects_Array_Access);

   type Files_For_Unit is record
      Spec_File, Body_File : aliased US.Unbounded_String;
   end record;
   --  Identify the source files that implement one unit (spec & body for a
   --  specific unit name, when present).

   procedure Set_Unit_File
     (FFU  : in out Files_For_Unit;
      File : String;
      Part : Any_Unit_Part);
   --  Register the couple File/Part in FFU

   package Unit_Files_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => Files_For_Unit,
      Equivalent_Keys => US."=",
      Hash            => US.Hash);
   --  Associate a set of files to unit names

   procedure Set_Unit_File
     (Unit_Files : in out Unit_Files_Maps.Map;
      Unit_Name  : String;
      Unit_Part  : Any_Unit_Part;
      Filename   : String);
   --  Wrapper around Set_Unit_File to register the couple Filename/Unit_Part
   --  in the appropriate Unit_Files' entry. Create such an entry if needed.

   type Aggregate_Part is record
      Projects   : View_Vectors.Vector;
      Unit_Files : Unit_Files_Maps.Map;
   end record;
   --  Group of projects that make up one part in the aggregated projects
   --  partition.

   function Part_Image (Part : Aggregate_Part) return String;
   --  Return a human-readable string that represent the set of projects in
   --  Part.

   type Aggregate_Part_Access is access all Aggregate_Part;
   procedure Free is new Ada.Unchecked_Deallocation
     (Aggregate_Part, Aggregate_Part_Access);

   function Try_Merge
     (Part       : in out Aggregate_Part;
      Project    : Any_View;
      Unit_Files : in out Unit_Files_Maps.Map) return Boolean;
   --  If all common unit names in ``Part.Unit_Files`` and ``Unit_Files`` are
   --  associated with the same source files, update ``Part`` so that
   --  ``Project`` is part of it, clear ``Unit_Files`` and return True. Do
   --  nothing and return False otherwise.

   package Aggregate_Part_Vectors is new Ada.Containers.Vectors
     (Positive, Aggregate_Part_Access);
   procedure Free (Partition : in out Aggregate_Part_Vectors.Vector);

   function Create_Project_Unit_Provider
     (Tree  : Any_Tree;
      Views : View_Vectors.Vector) return LAL.Unit_Provider_Reference;
   --  Helper to create a ``Project_Unit_Provider`` reference based on the
   --  given ``Tree`` and ``Views``.

   function Create_Project_Unit_Providers
     (Tree : Any_Tree) return Any_Provider_And_Projects_Array_Access;
   --  Common implementation for the homonym public functions

   procedure Create_Project_Unit_Provider
     (Tree         : Any_Tree;
      View         : Any_View;
      Provider     : out Project_Unit_Provider_Access;
      Provider_Ref : out LAL.Unit_Provider_Reference);
   --  Common implementation for the homonym public functions.
   --
   --  Try to create a single unit provider for the given ``Tree``/``View``
   --  (``View`` being ``No_View`` means: use the root project). On success,
   --  set ``Provider`` and ``Provider_Ref`` to the created unit provider. On
   --  failure, raise an ``Unsupported_View_Error`` exception.

   procedure Create_Sorted_Filenames
     (File_Set    : Filename_Sets.Set;
      File_Vector : out Filename_Vectors.Vector);
   --  Sort files in ``File_Set`` and put the result in ``File_Vector``

   function Default_Charset_From_Project
     (Tree : Any_Tree; View : Any_View) return String;
   --  Common implementation for the homonym public functions

   ---------------------------------
   -- Create_Context_From_Project --
   ---------------------------------

   function Create_Context_From_Project
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type := Prj.No_Project;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True;
      Event_Handler    : LAL.Event_Handler_Reference :=
                           LAL.No_Event_Handler_Ref;
      With_Trivia      : Boolean := True;
      Tab_Stop         : Positive := 8)
     return LAL.Analysis_Context
   is
      Result : Internal_Context := Allocate_Context;

      EH_Int : Internal_Event_Handler_Access :=
        Wrap_Public_Event_Handler (Event_Handler);
      --  This creates a ref-counted object: we must decrease its ref-count
      --  whether we return normally or through an exception.
   begin
      begin
         GPR_Impl.Initialize_Context_From_Project
           (Result,
            Tree,
            Project,
            Env,
            Is_Project_Owner,
            EH_Int,
            With_Trivia,
            Tab_Stop);
      exception
         when Unsupported_View_Error =>
            Dec_Ref (EH_Int);
            Release_Uninitialized_Context (Result);
            raise;
      end;
      Dec_Ref (EH_Int);

      return Context : constant LAL.Analysis_Context := Wrap_Context (Result)
      do
         --  Now that ``Context`` has one ownership share, release the one that
         --  ``Result`` has.

         Dec_Ref (Result);
      end return;
   end Create_Context_From_Project;

   -------------------
   -- Set_Unit_File --
   -------------------

   procedure Set_Unit_File
     (FFU  : in out Files_For_Unit;
      File : String;
      Part : Any_Unit_Part)
   is
      Unit_File : constant access US.Unbounded_String :=
        (case Part is
         when Unit_Spec => FFU.Spec_File'Access,
         when Unit_Body     => FFU.Body_File'Access);
   begin
      pragma Assert (Unit_File.all = US.Null_Unbounded_String);
      Unit_File.all :=
        (if File = ""
         then US.Null_Unbounded_String
         else US.To_Unbounded_String
                (+Create (+File).Full_Name (Normalize => True)));
   end Set_Unit_File;

   -------------------
   -- Set_Unit_File --
   -------------------

   procedure Set_Unit_File
     (Unit_Files : in out Unit_Files_Maps.Map;
      Unit_Name  : String;
      Unit_Part  : Any_Unit_Part;
      Filename   : String)
   is
      use Unit_Files_Maps;

      --  TODO??? Refactor to make a single lookup/insertion

      UN       : constant US.Unbounded_String :=
        US.To_Unbounded_String (Unit_Name);
      Pos      : Cursor := Unit_Files.Find (UN);
      Inserted : Boolean;
   begin
      if not Has_Element (Pos) then
         Unit_Files.Insert (UN, Pos, Inserted);
         pragma Assert (Inserted);
      end if;

      Set_Unit_File (Unit_Files.Reference (Pos), Filename, Unit_Part);
   end Set_Unit_File;

   ----------------
   -- Part_Image --
   ----------------

   function Part_Image (Part : Aggregate_Part) return String is
      use Ada.Strings.Unbounded;
      Image : Unbounded_String;
      First   : Boolean := True;
   begin
      Append (Image, "<");
      for View of Part.Projects loop
         if First then
            First := False;
         else
            Append (Image, ", ");
         end if;
         Append (Image, Name (View));
      end loop;
      Append (Image, ">");
      return To_String (Image);
   end Part_Image;

   ---------------
   -- Try_Merge --
   ---------------

   function Try_Merge
     (Part       : in out Aggregate_Part;
      Project    : Any_View;
      Unit_Files : in out Unit_Files_Maps.Map) return Boolean
   is
      use Unit_Files_Maps;
   begin
      --  If Part contains nothing yet, no need to do the costly overlap check:
      --  just move info there and return.

      if Part.Unit_Files.Is_Empty then
         Part.Projects.Append (Project);
         Part.Unit_Files.Move (Unit_Files);
         return True;
      end if;

      --  Otherwise, first check that Part.Unit_Files and Unit_Files don't have
      --  conflicting units.

      for Prj_Pos in Unit_Files.Iterate loop
         declare
            use Ada.Strings.Unbounded;
            Unit_Name : constant Unbounded_String := Key (Prj_Pos);
            Part_Pos  : constant Cursor := Part.Unit_Files.Find (Unit_Name);
         begin
            if Has_Element (Part_Pos)
               and then Unit_Files.Reference (Prj_Pos).Element.all
                        /= Part.Unit_Files.Reference (Part_Pos).Element.all
            then
               if Trace.Is_Active then
                  Trace.Trace
                    ("Found conflicting source files for unit "
                     & To_String (Unit_Name) & " in " & Name (Project)
                     & " and " & Part_Image (Part));
               end if;
               return False;
            end if;
         end;
      end loop;

      --  Finally merge Project and Unit_Files into Part

      Part.Projects.Append (Project);
      for Prj_Pos in Unit_Files.Iterate loop
         declare
            Dummy_Cursor   : Cursor;
            Dummy_Inserted : Boolean;
         begin
            Part.Unit_Files.Insert
              (Key (Prj_Pos), Element (Prj_Pos), Dummy_Cursor, Dummy_Inserted);
         end;
      end loop;

      return True;
   end Try_Merge;

   ----------
   -- Free --
   ----------

   procedure Free (Partition : in out Aggregate_Part_Vectors.Vector) is
   begin
      for Part of Partition loop
         Free (Part);
      end loop;
      Partition.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (PAP_Array : in out Provider_And_Projects_Array_Access) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Provider_And_Projects_Array, Provider_And_Projects_Array_Access);
   begin
      for PAP of PAP_Array.all loop
         Prj.Unchecked_Free (PAP.Projects);
      end loop;
      Deallocate (PAP_Array);
   end Free;

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   function Create_Project_Unit_Provider
     (Tree  : Any_Tree;
      Views : View_Vectors.Vector) return LAL.Unit_Provider_Reference
   is
      Provider : Project_Unit_Provider;
      Data     : Project_Data renames Provider.Data;
   begin
      case Tree.Kind is
         when GPR1_Kind =>
            Data :=
              (Kind                  => GPR1_Kind,
               GPR1_Tree             => Tree.GPR1_Value,
               GPR1_Env              => null,
               GPR1_Is_Project_Owner => False);

         when GPR2_Kind =>
            Data :=
              (Kind => GPR2_Kind, GPR2_Tree => Tree.GPR2_Value.Reference);
      end case;
      Provider.Projects := Views;
      return LAL.Create_Unit_Provider_Reference (Provider);
   end Create_Project_Unit_Provider;

   -----------------------------------
   -- Create_Project_Unit_Providers --
   -----------------------------------

   function Create_Project_Unit_Providers
     (Tree : Any_Tree) return Any_Provider_And_Projects_Array_Access
   is
      Partition : Aggregate_Part_Vectors.Vector;
   begin
      Trace.Increase_Indent ("Trying to partition " & Name (Root (Tree)));

      if Is_Aggregate_Project (Root (Tree)) then

         --  We have an aggregate project: partition aggregated projects so
         --  that each unit providers (associated to one exclusive set of
         --  projects) has visibility on only one version of a unit.

         declare
            Views : View_Vectors.Vector := Aggregated_Projects (Root (Tree));

            function "<" (Left, Right : Any_View) return Boolean is
              (Name (Left) < Name (Right));

            package Sorting is new View_Vectors.Generic_Sorting;
         begin
            --  Sort views by name so that our output is deterministic:
            --  Aggregated_Project does not specify the order of projects in
            --  its result.

            Sorting.Sort (Views);

            --  For each aggregated project...

            Aggregate_Iteration : for View of Views loop
               declare
                  --  List all units defined and keep track of which source
                  --  files implement them.

                  Unit_Files : Unit_Files_Maps.Map;

                  procedure Process
                    (Unit_Name : String;
                     Unit_Part : Any_Unit_Part;
                     Filename  : String);

                  -------------
                  -- Process --
                  -------------

                  procedure Process
                    (Unit_Name : String;
                     Unit_Part : Any_Unit_Part;
                     Filename  : String) is
                  begin
                     Set_Unit_File
                       (Unit_Files, Unit_Name, Unit_Part, Filename);
                  end Process;

                  New_Part_Needed : Boolean := True;
               begin
                  Iterate_Ada_Units (Tree, View, Process'Access);

                  --  Then look for a part whose units do not conflict with
                  --  Unit_Files. Create a new one if there is no such part.

                  Part_Lookup : for Part of Partition loop
                     if Try_Merge (Part.all, View, Unit_Files) then
                        New_Part_Needed := False;
                        exit Part_Lookup;
                     end if;
                  end loop Part_Lookup;

                  if New_Part_Needed then
                     declare
                        Part : constant Aggregate_Part_Access :=
                          new Aggregate_Part;
                        Success : constant Boolean :=
                          Try_Merge (Part.all, View, Unit_Files);
                     begin
                        pragma Assert (Success);
                        Partition.Append (Part);
                     end;
                  end if;
               end;
            end loop Aggregate_Iteration;
         end;

         --  If the partition is empty (there was no aggregated project),
         --  create one unit provider anyway: this provider will refer to an
         --  empty list of projects.

         if Partition.Is_Empty then
            Partition.Append (new Aggregate_Part);
         end if;

      else
         --  Project is not an aggregate project, so the partition is obvious:
         --  one part that contains the root project.

         declare
            Part : constant Aggregate_Part_Access := new Aggregate_Part;
         begin
            Part.Projects.Append (Root (Tree));
            Partition.Append (Part);
         end;
      end if;

      Trace.Decrease_Indent;

      --  For debuggability, log how the Tree was partitioned

      if Trace.Is_Active then
         Trace.Increase_Indent ("Input project partitioned into:");
         for Cur in Partition.Iterate loop
            declare
               N    : constant Positive :=
                  Aggregate_Part_Vectors.To_Index (Cur);
               Part : Aggregate_Part renames Partition.Element (N).all;
            begin
               Trace.Trace ("Part" & N'Image & ": " & Part_Image (Part));
            end;
         end loop;
         Trace.Decrease_Indent;
      end if;

      --  The partition is ready: turn each part into a unit provider and
      --  return the list.

      return Result : constant Any_Provider_And_Projects_Array_Access :=
         new Any_Provider_And_Projects_Array (1 .. Natural (Partition.Length))
      do
         for I in Result.all'Range loop
            declare
               Views : View_Vectors.Vector renames Partition (I).Projects;
            begin
               Result (I).Provider :=
                 Create_Project_Unit_Provider (Tree, Views);
               Result (I).Projects := Views;
            end;
         end loop;
         Free (Partition);
      end return;
   end Create_Project_Unit_Providers;

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   procedure Create_Project_Unit_Provider
     (Tree         : Any_Tree;
      View         : Any_View;
      Provider     : out Project_Unit_Provider_Access;
      Provider_Ref : out LAL.Unit_Provider_Reference)
   is
      Actual_View : Any_View := View;
   begin
      --  If no project was given, try to run the partitionner

      if Actual_View = No_View (Tree) then
         declare
            PAPs : Any_Provider_And_Projects_Array_Access :=
              Create_Project_Unit_Providers (Tree);
         begin
            if PAPs.all'Length > 1 then
               Free (PAPs);
               raise Unsupported_View_Error with "inconsistent units found";
            end if;

            --  We only have one provider: return it

            Provider_Ref := PAPs.all (PAPs.all'First).Provider;
            Provider :=
              Project_Unit_Provider_Access (Provider_Ref.Unchecked_Get);
            Free (PAPs);
            return;
         end;
      end if;

      --  Peel the aggregate project layers (if any) around Actual_View. If we
      --  find an aggregate project with more than one aggregated project, this
      --  is an unsupported case.

      while Is_Aggregate_Project (Actual_View) loop
         declare
            Subprojects : constant View_Vectors.Vector :=
              Aggregated_Projects (Actual_View);
         begin
            exit when Subprojects.Length /= 1;
            Actual_View := Subprojects.First_Element;
         end;
      end loop;

      if Is_Aggregate_Project (Actual_View) then
         raise Unsupported_View_Error with
            "selected project is aggregate and has more than one sub-project";
      end if;

      declare
         Views : View_Vectors.Vector;
      begin
         Views.Append (Actual_View);
         Provider_Ref := Create_Project_Unit_Provider (Tree, Views);
         Provider :=
           Project_Unit_Provider_Access (Provider_Ref.Unchecked_Get);
      end;
   end Create_Project_Unit_Provider;

   -----------------------------------
   -- Create_Project_Unit_Providers --
   -----------------------------------

   function Create_Project_Unit_Providers
     (Tree : Prj.Project_Tree_Access) return Provider_And_Projects_Array_Access
   is
      Result : Any_Provider_And_Projects_Array_Access :=
        Create_Project_Unit_Providers
          ((Kind => GPR1_Kind, GPR1_Value => Tree));
   begin
      --  Convert Result (GPR library agnostic data structure) into the return
      --  type (GPR1-specific data structure).

      return R : constant Provider_And_Projects_Array_Access :=
        new Provider_And_Projects_Array (Result.all'Range)
      do
         for I in R.all'Range loop
            R (I).Provider := Result (I).Provider;
            declare
               Projects : View_Vectors.Vector renames Result (I).Projects;
               P        : Prj.Project_Array_Access renames R (I).Projects;
            begin
               P := new Prj.Project_Array (1 .. Natural (Projects.Length));
               for I in P.all'Range loop
                  P (I) := Projects (I).GPR1_Value;
               end loop;
            end;
         end loop;
         Free (Result);
      end return;
   end Create_Project_Unit_Providers;

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   function Create_Project_Unit_Provider
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type := Prj.No_Project;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference
   is
      Provider : Project_Unit_Provider_Access;
   begin
      return Result : LAL.Unit_Provider_Reference do
         Create_Project_Unit_Provider
           (Tree         => (Kind => GPR1_Kind, GPR1_Value => Tree),
            View         => (Kind => GPR1_Kind, GPR1_Value => Project),
            Provider     => Provider,
            Provider_Ref => Result);
         Provider.Data.GPR1_Env := Env;
         Provider.Data.GPR1_Is_Project_Owner := Is_Project_Owner;
      end return;
   end Create_Project_Unit_Provider;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is
   begin
      --  Get_Unit_Location is supposed to handle all cases, so this should be
      --  dead code.

      return (raise Program_Error);
   end Get_Unit_Filename;

   -----------------------
   -- Get_Unit_Location --
   -----------------------

   overriding procedure Get_Unit_Location
     (Provider       : Project_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out US.Unbounded_String;
      PLE_Root_Index : in out Natural)
   is
      Str_Name  : constant String :=
        Libadalang.Unit_Files.Unit_String_Name (Name);
   begin
      case Provider.Data.Kind is
      when GPR1_Kind =>
         begin
            --  GNATCOLL.Projects does not provide the compilation unit index
            --  information: we have to assume that there are always at most
            --  one compilation unit per source file.

            PLE_Root_Index := 1;

            --  Look for a source file corresponding to Name/Kind in all
            --  projects associated to this Provider.

            GNAT.Task_Lock.Lock;

            --  Unlike what is documented, it's not because File_From_Unit
            --  returns an non-empty string that the unit does belong to the
            --  project, so we must also check Create_From_Project's result.

            for View of Provider.Projects loop
               declare
                  P    : constant Prj.Project_Type := View.GPR1_Value;
                  File : constant Filesystem_String := Prj.File_From_Unit
                    (Project   => P,
                     Unit_Name => Str_Name,
                     Part      => Convert (Kind),
                     Language  => "Ada");
               begin
                  if File'Length /= 0 then
                     declare
                        Path     : constant GNATCOLL.VFS.Virtual_File :=
                          Prj.Create_From_Project (P, File).File;
                        Fullname : constant String := +Path.Full_Name;
                     begin
                        if Fullname'Length /= 0 then
                           GNAT.Task_Lock.Unlock;
                           Filename := US.To_Unbounded_String (Fullname);
                           return;
                        end if;
                     end;
                  end if;
               end;
            end loop;

            GNAT.Task_Lock.Unlock;
         exception
            when others =>
               GNAT.Task_Lock.Unlock;
               raise;
         end;

      when GPR2_Kind =>
         declare
            procedure Set (SUI : GPR2.Unit.Source_Unit_Identifier);
            --  Set ``Filename`` and ``PLE_Root_Index`` from ``SUI``'s

            function Lookup (View : GPR2.Project.View.Object) return Boolean;
            --  If ``View`` contains the requested unit, return ``True`` and
            --  set ``Filename`` to the corresponding filename. Return
            --  ``False`` otherwise.

            Unit_Name : constant GPR2.Name_Type := GPR2.Name_Type (Str_Name);

            ---------
            -- Set --
            ---------

            procedure Set (SUI : GPR2.Unit.Source_Unit_Identifier) is
               use type GPR2.Unit_Index;
            begin
               --  GPR2 sets the CU index to 0 when there is no "at N" clause
               --  in the project file. This is equivalont to "at 1", which is
               --  what we need here since PLE_Root_Index is a Positive.

               Filename := US.To_Unbounded_String (SUI.Source.Value);
               PLE_Root_Index :=
                 (if SUI.Index = 0 then 1 else Positive (SUI.Index));
            end Set;

            ------------
            -- Lookup --
            ------------

            function Lookup (View : GPR2.Project.View.Object) return Boolean is
               Unit : constant GPR2.Project.Unit_Info.Object :=
                 View.Unit (Unit_Name);
            begin
               if Unit.Is_Defined then
                  case Kind is
                     when Unit_Specification =>
                        if Unit.Has_Spec then
                           Set (Unit.Spec);
                           return True;
                        end if;
                     when Unit_Body =>
                        if Unit.Has_Body then
                           Set (Unit.Main_Body);
                           return True;
                        end if;
                  end case;
               end if;

               return False;
            end Lookup;

            Tree : GPR2.Project.Tree.Object renames
              Provider.Data.GPR2_Tree.all;
         begin
            --  Look for all the requested unit in the closure of all the
            --  projects that this provider handles.

            for View of Provider.Projects loop
               for V of Closure (View.GPR2_Value) loop
                  if Lookup (V) then
                     return;
                  end if;
               end loop;
            end loop;

            --  Also look in the runtime project, if any

            if Tree.Has_Runtime_Project and then Lookup (Tree.Runtime_Project)
            then
               return;
            end if;
         end;
      end case;

      --  If we reach this point, we have not found a unit handled by this
      --  provider that matches the requested name/kind.

      Filename := US.Null_Unbounded_String;
      PLE_Root_Index := 1;
   end Get_Unit_Location;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider : Project_Unit_Provider;
      Context  : LAL.Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return LAL.Analysis_Unit'Class
   is
      --  Get_Unit_And_PLE_Root is supposed to handle all cases, so this should
      --  be dead code.

      pragma Unreferenced (Provider, Context, Name, Kind, Charset, Reparse);
   begin
      return (raise Program_Error);
   end Get_Unit;

   ---------------------------
   -- Get_Unit_And_PLE_Root --
   ---------------------------

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Project_Unit_Provider;
      Context        : LAL.Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out LAL.Analysis_Unit'Class;
      PLE_Root_Index : in out Natural)
   is
      Filename : US.Unbounded_String;
   begin
      Provider.Get_Unit_Location (Name, Kind, Filename, PLE_Root_Index);
      pragma Assert (PLE_Root_Index > 0);

      if US.Length (Filename) > 0 then
         Unit := LAL.Analysis_Unit'Class
           (Context.Get_From_File (US.To_String (Filename), Charset, Reparse));
      else
         declare
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Name, Kind);
            Kind_Name  : constant Text_Type :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant Text_Type :=
               "Could not find source file for " & Name & " (" & Kind_Name
               & ")";
         begin
            Unit := LAL.Analysis_Unit'Class
              (Context.Get_With_Error (Dummy_File, Error, Charset));
         end;
      end if;
   end Get_Unit_And_PLE_Root;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Project_Unit_Provider) is
   begin
      case Provider.Data.Kind is
         when GPR1_Kind =>
            declare
               Data : Project_Data renames Provider.Data;
            begin
               GNAT.Task_Lock.Lock;

               if Data.GPR1_Is_Project_Owner then
                  Prj.Unload (Data.GPR1_Tree.all);
                  Prj.Free (Data.GPR1_Tree);
                  Prj.Free (Data.GPR1_Env);
               end if;
               Data.GPR1_Tree := null;
               Data.GPR1_Env := null;
               Data.GPR1_Is_Project_Owner := False;

               GNAT.Task_Lock.Unlock;

            exception
               when others =>
                  GNAT.Task_Lock.Unlock;
                  raise;
            end;

         when GPR2_Kind =>
            null;
      end case;
   end Release;

   -----------------------------
   -- Create_Sorted_Filenames --
   -----------------------------

   procedure Create_Sorted_Filenames
     (File_Set    : Filename_Sets.Set;
      File_Vector : out Filename_Vectors.Vector)
   is
      package Sorting is new Filename_Vectors.Generic_Sorting
        ("<" => US."<");
   begin
      for F of File_Set loop

         --  Normalize the files to return to avoid discrepancies during
         --  testing if GPR1 does normalization and GPR2 does not or
         --  conversely.

         declare
            Original : constant String := +F.Full_Name;
            Normalized : constant String :=
              GNAT.OS_Lib.Normalize_Pathname (Original);
         begin
            File_Vector.Append (US.To_Unbounded_String (Normalized));
         end;
      end loop;
      Sorting.Sort (File_Vector);
   end Create_Sorted_Filenames;

   ------------------
   -- Source_Files --
   ------------------

   function Source_Files
     (Tree     : Prj.Project_Tree'Class;
      Mode     : Source_Files_Mode := Default;
      Projects : Prj.Project_Array := Prj.Empty_Project_Array)
      return Filename_Vectors.Vector
   is
      use GNATCOLL.Projects;

      Result : Filename_Sets.Set;

      procedure Include (P : Prj.Project_Type);
      --  Include sources that belong to ``P`` (according to ``Mode``) to
      --  ``Result``.

      procedure Append (F : Virtual_File);
      --  If ``F`` is an Ada source in the project tree, append it to
      --  ``Result``. This is considered the case if at least one project
      --  in the project tree considers this file as an Ada source.

      -------------
      -- Include --
      -------------

      procedure Include (P : Prj.Project_Type) is
         Recursive                : Boolean;
         Include_Externally_Built : Boolean;
         List                     : File_Array_Access;
      begin
         case Mode is
            when Default =>

               --  Go through all projects except externally built ones

               Recursive := True;
               Include_Externally_Built := False;

            when Root_Project =>

               --  Go through ``P`` only, regardless of whether it is
               --  externally built.

               Recursive := False;
               Include_Externally_Built := True;

            when Whole_Project | Whole_Project_With_Runtime =>

               --  Go through the whole project sub tree

               Recursive := True;
               Include_Externally_Built := True;
         end case;
         List := P.Source_Files
           (Recursive                => Recursive,
            Include_Externally_Built => Include_Externally_Built);
         for F of List.all loop
            Append (F);
         end loop;
         Unchecked_Free (List);
      end Include;

      ------------
      -- Append --
      ------------

      procedure Append (F : Virtual_File) is
         FIS : constant File_Info_Set := Tree.Info_Set (F);
         --  Compute the set of ``File_Info`` for this file (one for each
         --  specific project of the project tree that includes this source).
         --  We use ``Tree.Info_Set`` instead of ``Tree.Info`` in order to
         --  support the case of aggregate projects. In such case, the set
         --  might contain multiple elements (one for each aggregated project
         --  that includes this source file): we choose to consider this file
         --  an Ada source if any of those projects considers it an Ada source.
         --
         --  TODO??? We could be more precise by checking that the actual
         --  project found in the ``File_Info`` is the one we are currently
         --  traversing, but it might actually not be if this source was found
         --  as part of a recursive lookup. Besides, it sounds unrealistic for
         --  a given source file to be considered an Ada source file in one
         --  subproject but, say, a C source file in another. This
         --  approximation should be good enough in practice, and will be
         --  deprecated anyway once the transition to GPR2 is complete.
      begin
         for FI of FIS loop
            if File_Info (FI).Language = "ada" then
               Result.Include (F);
               return;
            end if;
         end loop;
      end Append;

   begin
      --  Include sources from all the requested projects themselves

      if Projects'Length = 0 then
         Include (Tree.Root_Project);
      else
         for P of Projects loop
            Include (P);
         end loop;
      end if;

      --  Only then, if requested, get runtime sources: they are common to all
      --  subprojects.

      if Mode = Whole_Project_With_Runtime then
         declare
            Env : constant Project_Environment_Access :=
              Get_Environment (Tree.Root_Project);
         begin
            for F of Predefined_Source_Files (Env) loop
               Append (F);
            end loop;
         end;
      end if;

      --  Return the sorted list of source files. Sorting gets the output
      --  deterministic and thus helps reproducibility.

      return V : Filename_Vectors.Vector do
         Create_Sorted_Filenames (Result, V);
      end return;
   end Source_Files;

   ------------------
   -- Source_Files --
   ------------------

   function Source_Files
     (Tree     : GPR2.Project.Tree.Object;
      Mode     : Source_Files_Mode := Default;
      Projects : GPR2.Project.View.Set.Object := GPR2.Project.View.Set.Empty)
      return Filename_Vectors.Vector
   is
      --  Note that the GNATCOLL.Projects and GPR2 APIs to query source files
      --  are just too different, so creating a common API on top of them is
      --  not worth it. For this reason, this GPR2 implementation of
      --  Source_Files is completely independent.

      Result : Filename_Sets.Set;

      procedure Process (View : GPR2.Project.View.Object);
      --  Include sources that belong to ``P`` (according to ``Mode``) to
      --  ``Result``.

      procedure Include (View : GPR2.Project.View.Object);
      --  Include in ``Result`` all sources that directly belong to ``P``

      -------------
      -- Process --
      -------------

      procedure Process (View : GPR2.Project.View.Object) is
      begin
         case Mode is
            when Default =>

               --  Go through all projects except externally built ones

               for V of Closure (View) loop
                  if not V.Is_Externally_Built then
                     Include (V);
                  end if;
               end loop;

            when Root_Project =>

               --  Go through ``P`` only, regardless of whether it is
               --  externally built.

               Include (View);

            when Whole_Project | Whole_Project_With_Runtime =>

               --  Go through the whole project sub tree

               for V of Closure (View) loop
                  Include (V);
               end loop;
         end case;
      end Process;

      -------------
      -- Include --
      -------------

      procedure Include (View : GPR2.Project.View.Object) is
         use type GPR2.Language_Id;
      begin
         for S of View.Sources loop
            if S.Language = GPR2.Ada_Language then
               Result.Include (Create (+String (S.Path_Name.Value)));
            end if;
         end loop;
      end Include;

   begin
      --  Include sources from all the requested projects themselves

      if Projects.Is_Empty then
         Process (Tree.Root_Project);
      else
         for P of Projects loop
            Process (P);
         end loop;
      end if;

      --  Only then, if requested, get runtime sources: they are common to all
      --  subprojects.

      if Mode = Whole_Project_With_Runtime and then Tree.Has_Runtime_Project
      then
         Include (Tree.Runtime_Project);
      end if;

      --  Return the sorted list of source files. Sorting gets the output
      --  deterministic and thus helps reproducibility.

      return V : Filename_Vectors.Vector do
         Create_Sorted_Filenames (Result, V);
      end return;
   end Source_Files;

   -----------------------------------
   -- Create_Project_Unit_Providers --
   -----------------------------------

   function Create_Project_Unit_Providers
     (Tree : GPR2.Project.Tree.Object)
      return GPR2_Provider_And_Projects_Array_Access
   is
      Result : Any_Provider_And_Projects_Array_Access :=
        Create_Project_Unit_Providers
          ((Kind => GPR2_Kind, GPR2_Value => Tree.Reference));
   begin
      --  Convert Result (GPR library agnostic data structure) into the return
      --  type (GPR2-specific data structure).

      return R : constant GPR2_Provider_And_Projects_Array_Access :=
        new GPR2_Provider_And_Projects_Array (Result.all'Range)
      do
         for I in R.all'Range loop
            R (I).Provider := Result (I).Provider;
            declare
               Projects : View_Vectors.Vector renames Result (I).Projects;
               P        : GPR2.Project.View.Vector.Object renames
                 R (I).Projects;
            begin
               for V of Projects loop
                  P.Append (V.GPR2_Value);
               end loop;
            end;
         end loop;
         Free (Result);
      end return;
   end Create_Project_Unit_Providers;

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   function Create_Project_Unit_Provider
     (Tree             : GPR2.Project.Tree.Object;
      Project          : GPR2.Project.View.Object :=
                           GPR2.Project.View.Undefined)
      return LAL.Unit_Provider_Reference
   is
      Dummy : Project_Unit_Provider_Access;
   begin
      return Result : LAL.Unit_Provider_Reference do
         Create_Project_Unit_Provider
           (Tree         => (Kind => GPR2_Kind, GPR2_Value => Tree.Reference),
            View         => (Kind => GPR2_Kind, GPR2_Value => Project),
            Provider     => Dummy,
            Provider_Ref => Result);
      end return;
   end Create_Project_Unit_Provider;

   ----------------------------------
   -- Default_Charset_From_Project --
   ----------------------------------

   function Default_Charset_From_Project
     (Tree : Any_Tree; View : Any_View) return String
   is
      UTF8 : Boolean := False;

      procedure Process_Switch (View : Any_View; Switch : XString);
      --  If ``Switch`` is ``-gnatW8``, set ``UTF8`` to True

      --------------------
      -- Process_Switch --
      --------------------

      procedure Process_Switch (View : Any_View; Switch : XString) is
         pragma Unreferenced (View);
      begin
         if Switch = "-gnatW8" then
            UTF8 := True;
         end if;
      end Process_Switch;

   begin
      Iterate_Ada_Compiler_Switches (Tree, View, Process_Switch'Access);
      return (if UTF8 then "utf-8" else Default_Charset);
   end Default_Charset_From_Project;

   ----------------------------------
   -- Default_Charset_From_Project --
   ----------------------------------

   function Default_Charset_From_Project
     (Tree    : Prj.Project_Tree'Class;
      Project : Prj.Project_Type := Prj.No_Project) return String is
   begin
      return Default_Charset_From_Project
        (Tree => (Kind => GPR1_Kind, GPR1_Value => Tree'Unrestricted_Access),
         View => (Kind => GPR1_Kind, GPR1_Value => Project));
   end Default_Charset_From_Project;

   ----------------------------------
   -- Default_Charset_From_Project --
   ----------------------------------

   function Default_Charset_From_Project
     (Tree    : GPR2.Project.Tree.Object;
      Project : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return String is
   begin
      return Default_Charset_From_Project
        (Tree => (Kind => GPR2_Kind, GPR2_Value => Tree'Unrestricted_Access),
         View => (Kind => GPR2_Kind, GPR2_Value => Project));
   end Default_Charset_From_Project;

   ---------------------------------
   -- Create_Context_From_Project --
   ---------------------------------

   function Create_Context_From_Project
     (Tree          : GPR2.Project.Tree.Object;
      Project       : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Event_Handler : LAL.Event_Handler_Reference := LAL.No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
     return LAL.Analysis_Context
   is
      Result : Internal_Context := Allocate_Context;

      EH_Int : Internal_Event_Handler_Access :=
        Wrap_Public_Event_Handler (Event_Handler);
      --  This creates a ref-counted object: we must decrease its ref-count
      --  whether we return normally or through an exception.
   begin
      begin
         GPR_Impl.Initialize_Context_From_Project
           (Result, Tree, Project, EH_Int, With_Trivia, Tab_Stop);
      exception
         when Unsupported_View_Error =>
            Dec_Ref (EH_Int);
            Release_Uninitialized_Context (Result);
            raise;
      end;
      Dec_Ref (EH_Int);

      return Context : constant LAL.Analysis_Context := Wrap_Context (Result)
      do
         --  Now that ``Context`` has one ownership share, release the one that
         --  ``Result`` has.

         Dec_Ref (Result);
      end return;
   end Create_Context_From_Project;

end Libadalang.Project_Provider;
