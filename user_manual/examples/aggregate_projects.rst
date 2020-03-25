.. _examples_aggregate_projects:

Dealing with aggregate projects
###############################

Processing aggregate projects with Libadalang is peculiar. Please get familiar
with the :ref:`unit-providers-aggregate` section before reading this.

General examples
================

Input sources
*************

Get the projects files and sources from the :ref:`unit-providers-aggregate`
section.

Sample code
***********

Ada
---

.. code-block:: ada

   with GNATCOLL.Projects;
   with GNATCOLL.VFS;
   with Libadalang.Analysis;
   with Libadalang.Project_Provider;

   --  Create a unit provider for the "arch32/arch.gpr" project, loaded through
   --  the "agg.gpr" project.
   declare
      package Prj renames GNATCOLL.Projects;
      package VFS renames GNATCOLL.VFS;
      package LAL renames Libadalang.Analysis;

      PT  : Prj.Project_Tree_Access;
      Env : Prj.Project_Environment_Access;
      P   : Prj.Project_Type;

      UP  : LAL.Unit_Provider_Reference;
      Ctx : LAL.Analysis_Context;
   begin
      --  Load the "agg.gpr" project
      PT := new Prj.Project_Tree;
      Prj.Initialize (Env);
      PT.Load (Root_Project_Path => VFS.Create ("agg.gpr"),
               Env               => Env);

      --  Fetch the "arch/arch32.gpr" project
      P := PT.Project_From_Path (VFS.Create ("arch/arch32.gpr"));

      --  Create the unit provider, then the analysis context
      UP := Libadalang.Project_Provider.Create_Project_Unit_Provider
        (PT, P, Env);
      Ctx := LAL.Create_Context (Unit_Provider => UP);
   end;

   --  Create several project providers so that one provider has at most one
   --  source file per unit name/unit kind.
   declare
      package Prj renames GNATCOLL.Projects;
      package VFS renames GNATCOLL.VFS;
      package LAL renames Libadalang.Analysis;

      PT   : Prj.Project_Tree_Access;
      Env  : Prj.Project_Environment_Access;
      PAPs : Libadalang.Project_Provider.Provider_And_Projects_Array_Access;
   begin
      --  Load the "agg.gpr" project
      PT := new Prj.Project_Tree;
      Prj.Initialize (Env);
      PT.Load (Root_Project_Path => VFS.Create ("agg.gpr"),
               Env               => Env);

      --  Create one analysis context per unit provider
      PAPs := Libadalang.Project_Provider.Create_Project_Unit_Providers (PT);
      for PAP of PAPs.all loop
         declare
            Ctx : constant LAL.Analysis_Context :=
               LAL.Create_Context (Unit_Provider => PAP.Provider);
         begin
            --  Do your processings...
            null;
         end;
      end loop;
      Libadalang.Project_Provider.Free (PAPs);

      --  Free allocated resources
      Prj.Free (PT);
      Prj.Free (Env);
   end;

Python
------

.. code-block:: python

   # Create a unit provider for the "arch32/arch.gpr" project, loaded through
   # the "agg.gpr" project. Then create an analysis context using it.
   up = libadalang.UnitProvider.for_project(
      'agg.gpr', project='arch32/arch.gpr'
   )

   ctx = libadalang.AnalysisContext(unit_provider=up)


Creating one provide/context for each aggregated project
========================================================

This complete example demonstrates how to use the ``GNATCOLL.Projects`` API in
order to analyze each aggregated project separately with Libadalang.

Note that there is no equivalent in Python as the GNATCOLL project does not
provide a Python API to analyze projects files.

Input sources
*************

.. code-block:: ada

   --  arch32/arch.gpr
   project Arch is
      for Object_Dir use "obj";
   end Arch;

   --  arch32/arch.ads
   package Arch is
      type Target_Address is mod 2 ** 32;
   end Arch;

   --  arch64/arch.gpr
   package Arch is
      type Target_Address is mod 2 ** 64;
   end Arch;

   --  arch64/arch.ads
   project Arch is
      for Object_Dir use "obj";
   end Arch;

   --  main/main32.gpr
   with "../arch32/arch";
   project Main32 is
      for Object_Dir use "obj-32";
      for Main use ("main");
   end Main32;

   --  main/main64.gpr
   with "../arch64/arch";
   project Main64 is
      for Object_Dir use "obj-64";
      for Main use ("main");
   end Main64;

   --  main/main.adb
   with Ada.Text_IO, Arch;
   procedure Main is
      Last : constant Arch.Target_Address := Arch.Target_Address'Last;
   begin
      Ada.Text_IO.Put_Line ("Arch.Target_Address'Last =" & Last'Image);
   end;

   --  agg32.gpr
   aggregate project Agg32 is
      for Project_Files use ("main/main32.gpr");
   end Agg32;

   --  agg64.gpr
   aggregate project Agg64 is
      for Project_Files use ("main/main64.gpr");
   end Agg64;

   --  agg.gpr
   aggregate project Agg is
      for Project_Files use ("agg32.gpr", "agg64.gpr");
   end Agg;


Sample code
***********

.. code-block:: ada

   with Ada.Containers.Generic_Array_Sort;
   with Ada.Text_IO; use Ada.Text_IO;

   with GNATCOLL.Projects; use GNATCOLL.Projects;
   with GNATCOLL.VFS;      use GNATCOLL.VFS;

   with Libadalang.Analysis;         use Libadalang.Analysis;
   with Libadalang.Common;           use Libadalang.Common;
   with Libadalang.Project_Provider; use Libadalang.Project_Provider;

   procedure Run is

      --  Sort lists of projects and source files so that execution is
      --  deterministic.

      function "<" (Left, Right : Project_Type) return Boolean is
        (Left.Project_Path < Right.Project_Path);

      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type   => Positive,
         Element_Type => Virtual_File,
         Array_Type   => File_Array);
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type   => Positive,
         Element_Type => Project_Type,
         Array_Type   => Project_Array);

      procedure Iterate_Aggregated
        (Project  : Project_Type;
         Callback : access procedure (Project : Project_Type));
      --  If Project is not an aggregate project, just call Callback on it.
      --  Otherwise, recurse on Project's aggregated projects.

      procedure Process_Aggregated (Project : Project_Type);
      --  Analyze all sources in Project

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      --  If Node is an object declaration, show the corresponding type
      --  declaration node.

      Env  : Project_Environment_Access;
      Tree : Project_Tree_Access := new Project_Tree;

      ------------------------
      -- Iterate_Aggregated --
      ------------------------

      procedure Iterate_Aggregated
        (Project  : Project_Type;
         Callback : access procedure (Project : Project_Type))
      is
      begin
         if Project.Is_Aggregate_Project then
            declare
               Aggregated : Project_Array_Access := Project.Aggregated_Projects;
            begin
               Sort (Aggregated.all);
               for A of Aggregated.all loop
                  Iterate_Aggregated (A, Callback);
               end loop;
               Unchecked_Free (Aggregated);
            end;

         else
            Callback (Project);
         end if;
      end Iterate_Aggregated;

      ------------------------
      -- Process_Aggregated --
      ------------------------

      procedure Process_Aggregated (Project : Project_Type) is
         --  Create an analysis context that has a view on all sources in Project

         UP   : constant Unit_Provider_Reference := Create_Project_Unit_Provider
           (Tree, Project, Env,

            --  We handle the lifetime of Tree and Env manually (see the Free
            --  calls at the end of this source file), so consider that UP won't
            --  own them.
            Is_Project_Owner => False);

         Context : constant Analysis_Context :=
            Create_Context (Unit_Provider => UP);
         Sources : File_Array_Access := Project.Source_Files (Recursive => True);
      begin
         Put_Line ("== Processing project: " & Project.Name & " ==");

         Sort (Sources.all);

         for Filename of Sources.all loop
            declare
               Unit : constant Analysis_Unit :=
                  Context.Get_From_File (+Filename.Full_Name);
            begin
               Put_Line ("In " & (+Filename.Base_Name) & ":");
               Unit.Root.Traverse (Process_Node'Access);
            end;
         end loop;

         Unchecked_Free (Sources);
         New_Line;
      end Process_Aggregated;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Object_Decl then
            declare
               Type_Decl : constant Base_Type_Decl :=
                  Node.As_Object_Decl.F_Type_Expr.P_Designated_Type_Decl;
            begin
               Put_Line ("  Type for " & Node.Short_Image
                         & ": " & Type_Decl.Debug_Text);
            end;
         end if;

         return Into;
      end Process_Node;

   begin
      --  Load the input project
      Initialize (Env);
      Tree.Load (Create (+"agg.gpr"), Env);

      --  Process every aggregated project independently
      Iterate_Aggregated (Tree.Root_Project, Process_Aggregated'Access);

      --  Do not leak project resources
      Free (Tree);
      Free (Env);
   end Run;

Expected output
***************

.. code-block:: text

   == Processing project: Main32 ==
   In arch.ads:
   In main.adb:
     Type for <ObjectDecl ["Last"] main.adb:3:4-3:68>: type Target_Address is mod 2 ** 32;

   == Processing project: Main64 ==
   In arch.ads:
   In main.adb:
     Type for <ObjectDecl ["Last"] main.adb:3:4-3:68>: type Target_Address is mod 2 ** 64;
