------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2019, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Locks;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.GPR_Lock;
with Libadalang.Unit_Files;

package body Libadalang.Project_Provider is

   package US renames Ada.Strings.Unbounded;
   use type US.Unbounded_String;

   type Project_Unit_Provider is new LAL.Unit_Provider_Interface with record
      Tree             : Prj.Project_Tree_Access;
      Projects         : Prj.Project_Array_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean;
   end record;
   --  Unit provider backed up by a project file

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class;

   overriding procedure Release (Provider : in out Project_Unit_Provider);

   ------------------------------------------
   -- Helpers to create project partitions --
   ------------------------------------------

   type Files_For_Unit is record
      Spec_File, Body_File : aliased US.Unbounded_String;
   end record;
   --  Identify the source files that implement one unit (spec & body for a
   --  specific unit name, when present).

   procedure Set_Unit_File
     (FFU  : in out Files_For_Unit;
      File : Virtual_File;
      Part : Prj.Unit_Parts);
   --  Register the couple File/Part in FFU

   package Unit_Files_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => Files_For_Unit,
      Equivalent_Keys => US."=",
      Hash            => US.Hash);
   --  Associate a set of files to unit names

   procedure Set_Unit_File
     (Unit_Files : in out Unit_Files_Maps.Map;
      Tree       : Prj.Project_Tree_Access;
      File       : Virtual_File);
   --  Wrapper around Set_Unit_File to register the couple File/Part in the
   --  appropriate Unit_Files' entry. Create such an entry if needed.

   package Project_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Prj.Project_Type,
      "="          => Prj."=");

   function To_Project_Array
     (Projects : Project_Vectors.Vector) return Prj.Project_Array_Access;

   type Aggregate_Part is record
      Projects   : Project_Vectors.Vector;
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
      Project    : Prj.Project_Type;
      Unit_Files : in out Unit_Files_Maps.Map) return Boolean;
   --  If all common unit names in Part.Unit_Files and Unit_Files are
   --  associated with the same source files, update Part so that Project is
   --  part of it, clear Unit_Files and return True. Do nothing and return
   --  False otherwise.

   package Aggregate_Part_Vectors is new Ada.Containers.Vectors
     (Positive, Aggregate_Part_Access);
   procedure Free (Partition : in out Aggregate_Part_Vectors.Vector);

   -------------------
   -- Set_Unit_File --
   -------------------

   procedure Set_Unit_File
     (FFU  : in out Files_For_Unit;
      File : Virtual_File;
      Part : Prj.Unit_Parts)
   is
      Unit_File : constant access US.Unbounded_String :=
        (case Part is
         when Prj.Unit_Spec => FFU.Spec_File'Access,
         when others        => FFU.Body_File'Access);
   begin
      pragma Assert (Unit_File.all = US.Null_Unbounded_String);
      Unit_File.all :=
        (if File = No_File
         then US.Null_Unbounded_String
         else US.To_Unbounded_String (+File.Full_Name (Normalize => True)));
   end Set_Unit_File;

   -------------------
   -- Set_Unit_File --
   -------------------

   procedure Set_Unit_File
     (Unit_Files : in out Unit_Files_Maps.Map;
      Tree       : Prj.Project_Tree_Access;
      File       : Virtual_File)
   is
      use type Prj.Project_Type;
      use Unit_Files_Maps;
   begin
      --  Look for the file info that corresponds to File.
      --
      --  TODO??? Due to how GNATCOLL.Projects exposes aggregate projects, we
      --  have no way to get the unit name and unit part from File without
      --  performing a project tree wide search: we would like instead to
      --  search on Project only, but this is not possible. For now, just do
      --  the global search and hope that File always corresponds to the same
      --  unit file and unit part in the aggregate project. While this sounds a
      --  reasonable assumption, we know it's possible to build a project with
      --  unlikely Name package attribute that break this assumption.

      declare
         Set : constant Prj.File_Info_Set := Tree.Info_Set (File);
         FI  : constant Prj.File_Info := Prj.File_Info (Set.First_Element);
         --  For some reason, File_Info_Set contains File_Info_Astract'Class
         --  objects, while the only instance of this type is File_Info. So the
         --  above conversion should always succeed.
      begin
         --  Info_Set returns a project-less file info when called of files
         --  that are not part of the project tree. Here, all our source files
         --  belong to Tree, so the following assertion should hold.

         pragma Assert (FI.Project /= Prj.No_Project);

         --  Now look for the Files_For_Unit entry in Unit_Files corresponding
         --  to this file and register it there.

         declare
            Unit_Name : constant US.Unbounded_String :=
               US.To_Unbounded_String (FI.Unit_Name);
            Unit_Part : constant Prj.Unit_Parts := FI.Unit_Part;

            Pos      : Cursor := Unit_Files.Find (Unit_Name);
            Inserted : Boolean;
         begin
            if not Has_Element (Pos) then
               Unit_Files.Insert (Unit_Name, Pos, Inserted);
               pragma Assert (Inserted);
            end if;

            Set_Unit_File (Unit_Files.Reference (Pos), File, Unit_Part);
         end;
      end;
   end Set_Unit_File;

   ----------------------
   -- To_Project_Array --
   ----------------------

   function To_Project_Array
     (Projects : Project_Vectors.Vector) return Prj.Project_Array_Access is
   begin
      return Result : constant Prj.Project_Array_Access :=
         new Prj.Project_Array (1 .. Natural (Projects.Length))
      do
         for I in Result.all'Range loop
            Result (I) := Projects.Element (I);
         end loop;
      end return;
   end To_Project_Array;

   ----------------
   -- Part_Image --
   ----------------

   function Part_Image (Part : Aggregate_Part) return String is
      use Ada.Strings.Unbounded;
      Image : Unbounded_String;
      First   : Boolean := True;
   begin
      Append (Image, "<");
      for Project of Part.Projects loop
         if First then
            First := False;
         else
            Append (Image, ", ");
         end if;
         Append (Image, Project.Name);
      end loop;
      Append (Image, ">");
      return To_String (Image);
   end Part_Image;

   ---------------
   -- Try_Merge --
   ---------------

   function Try_Merge
     (Part       : in out Aggregate_Part;
      Project    : Prj.Project_Type;
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
                     & To_String (Unit_Name) & " in " & Project.Name & " and "
                     & Part_Image (Part));
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

   -----------------------------------
   -- Create_Project_Unit_Providers --
   -----------------------------------

   function Create_Project_Unit_Providers
     (Tree : Prj.Project_Tree_Access)
      return Provider_And_Projects_Array_Access
   is
      Partition : Aggregate_Part_Vectors.Vector;
   begin
      Trace.Increase_Indent ("Trying to partition " & Tree.Root_Project.Name);

      if Tree.Root_Project.Is_Aggregate_Project then

         --  We have an aggregate project: partition aggregated projects so
         --  that each unit providers (associated to one exclusive set of
         --  projects) has visibility on only one version of a unit.

         declare
            Projects : Prj.Project_Array_Access :=
               Tree.Root_Project.Aggregated_Projects;

            function "<" (Left, Right : Prj.Project_Type) return Boolean is
              (Left.Name < Right.Name);

            procedure Sort is new Ada.Containers.Generic_Array_Sort
              (Positive, Prj.Project_Type, Prj.Project_Array);
         begin
            --  Sort projects by name so that our output is deterministic:
            --  GNATCOLL.Projects.Aggregated_Project does not specify the order
            --  of projects in its result.

            Sort (Projects.all);

            --  For each aggregated project...

            Aggregate_Iteration : for P of Projects.all loop
               declare
                  Unit_Files      : Unit_Files_Maps.Map;
                  Sources         : File_Array_Access :=
                     P.Source_Files (Recursive => True);
                  New_Part_Needed : Boolean := True;
               begin
                  --  List all units defined and keep track of which source
                  --  files implement them.

                  for S of Sources.all loop
                     Set_Unit_File (Unit_Files, Tree, S);
                  end loop;
                  Unchecked_Free (Sources);

                  --  Then look for a part whose units do not conflict with
                  --  Unit_Files. Create a new one if there is no such part.

                  Part_Lookup : for Part of Partition loop
                     if Try_Merge (Part.all, P, Unit_Files) then
                        New_Part_Needed := False;
                        exit Part_Lookup;
                     end if;
                  end loop Part_Lookup;

                  if New_Part_Needed then
                     declare
                        Part : constant Aggregate_Part_Access :=
                           new Aggregate_Part;
                        Success : constant Boolean :=
                           Try_Merge (Part.all, P, Unit_Files);
                     begin
                        pragma Assert (Success);
                        Partition.Append (Part);
                     end;
                  end if;
               end;
            end loop Aggregate_Iteration;
            Prj.Unchecked_Free (Projects);
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
            Part.Projects.Append (Tree.Root_Project);
            Partition.Append (Part);
         end;
      end if;

      Trace.Decrease_Indent;

      --  For debuggability, log how the Tree was partitionned

      if Trace.Is_Active then
         Trace.Increase_Indent ("Input project partitionned into:");
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

      return Result : constant Provider_And_Projects_Array_Access :=
         new Provider_And_Projects_Array (1 .. Natural (Partition.Length))
      do
         for I in Result.all'Range loop
            declare
               Part : Aggregate_Part_Access renames Partition (I);
               PUP  : constant Project_Unit_Provider :=
                  (Tree             => Tree,
                   Projects         => To_Project_Array (Part.Projects),
                   Env              => null,
                   Is_Project_Owner => False);
            begin
               Result (I).Projects := To_Project_Array (Part.Projects);
               Result (I).Provider :=
                  LAL.Create_Unit_Provider_Reference (PUP);
            end;
         end loop;
         Free (Partition);
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
      use type Prj.Project_Type;

      Actual_Project : Prj.Project_Type := Project;
   begin
      --  If no project was given, peel the aggregate project layers (if
      --  any) around Tree's root project.

      if Actual_Project = Prj.No_Project then
         Actual_Project := Tree.Root_Project;
         while Actual_Project.Is_Aggregate_Project loop
            declare
               Subprojects : Prj.Project_Array_Access :=
                  Actual_Project.Aggregated_Projects;
               Leave_Loop  : constant Boolean :=
                  Subprojects.all'Length /= 1;
            begin
               if not Leave_Loop then
                  Actual_Project := Subprojects.all (Subprojects.all'First);
               end if;
               Prj.Unchecked_Free (Subprojects);
               exit when Leave_Loop;
            end;
         end loop;
      end if;

      if Actual_Project.Is_Aggregate_Project then
         raise Prj.Invalid_Project with
            "aggregate project has too many sub-projects";
      end if;

      declare
         Provider : constant Project_Unit_Provider :=
           (Tree             => Tree,
            Projects         => new Prj.Project_Array'(1 => Actual_Project),
            Env              => Env,
            Is_Project_Owner => Is_Project_Owner);
      begin
         return LAL.Create_Unit_Provider_Reference (Provider);
      end;
   end Create_Project_Unit_Provider;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);

      Str_Name : constant String :=
        Libadalang.Unit_Files.Unit_String_Name (Name);
   begin
      --  Look for a source file corresponding to Name/Kind in all projects
      --  associated to this Provider. Note that unlike what is documented,
      --  it's not because File_From_Unit returns an non-empty string that the
      --  unit does belong to the project, so we must also check
      --  Create_From_Project's result.

      for P of Provider.Projects.all loop
         declare
            File : constant Filesystem_String := Prj.File_From_Unit
              (Project   => P,
               Unit_Name => Str_Name,
               Part      => Convert (Kind),
               Language  => "Ada");
         begin
            if File'Length /= 0 then
               declare
                  Path : constant GNATCOLL.VFS.Virtual_File :=
                    Prj.Create_From_Project (P, File).File;
                  Fullname : constant String := +Path.Full_Name;
               begin
                  if Fullname'Length /= 0 then
                     return Fullname;
                  end if;
               end;
            end if;
         end;
      end loop;

      return "";
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return LAL.Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Str_Name : constant String :=
               Libadalang.Unit_Files.Unit_String_Name (Name);
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Str_Name, Kind);
            Kind_Name  : constant String :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant String :=
               "Could not find source file for " & Str_Name & " (" & Kind_Name
               & ")";
         begin
            return LAL.Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Project_Unit_Provider)
   is
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);
   begin
      Prj.Unchecked_Free (Provider.Projects);
      if Provider.Is_Project_Owner then
         Prj.Unload (Provider.Tree.all);
         Prj.Free (Provider.Tree);
         Prj.Free (Provider.Env);
      end if;
      Provider.Tree := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Release;

end Libadalang.Project_Provider;
