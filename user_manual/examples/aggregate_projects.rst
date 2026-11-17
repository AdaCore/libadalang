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

   with GPR2.Options;
   with GPR2.Project.Tree;
   with GPR2.Project.View;
   with Libadalang.Analysis;
   with Libadalang.Project_Provider;

   --  Create a unit provider for the "arch32/arch.gpr" project, loaded through
   --  the "agg.gpr" project.
   declare
      package LAL renames Libadalang.Analysis;

      Opts : GPR2.Options.Object;
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object;

      UP  : LAL.Unit_Provider_Reference;
      Ctx : LAL.Analysis_Context;
   begin
      --  Load the "agg.gpr" project
      Opts.Add_Switch (GPR2.Options.P, "agg.gpr");
      if not Tree.Load
        (Opts,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error with "error loading the project tree";
      end if;

      --  Fetch the "arch32/arch32.gpr" project
      for V of Tree loop
         if V.Dir_Name.Simple_Name = "arch32" then
            View := V;
            exit;
         end if;
      end loop;

      --  Create the unit provider, then the analysis context
      UP := Libadalang.Project_Provider.Create_Project_Unit_Provider
        (Tree, View);
      Ctx := LAL.Create_Context (Unit_Provider => UP);
   end;

   --  Create several project providers so that one provider has at most one
   --  source file per unit name/unit kind.
   declare
      package LAL renames Libadalang.Analysis;
      package LALGPR renames Libadalang.Project_Provider;

      Opts : GPR2.Options.Object;
      Tree : GPR2.Project.Tree.Object;
      PAPs : LALGPR.GPR2_Provider_And_Projects_Array_Access;
   begin
      --  Load the "agg.gpr" project
      Opts.Add_Switch (GPR2.Options.P, "agg.gpr");
      if not Tree.Load
        (Opts,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error with "error loading the project tree";
      end if;

      --  Create one analysis context per unit provider
      PAPs := LALGPR.Create_Project_Unit_Providers (Tree);
      for PAP of PAPs.all loop
         declare
            Ctx : constant LAL.Analysis_Context :=
               LAL.Create_Context (Unit_Provider => PAP.Provider);
         begin
            --  Do your processings...
            null;
         end;
      end loop;
      LALGPR.Free (PAPs);
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

This complete example demonstrates how to use the ``GPR2`` API in order to
analyze each aggregated project separately with Libadalang.

Note that there is no equivalent in Python as the GPR2 project does not provide
a Python API to analyze projects files.

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
   project Arch is
      for Object_Dir use "obj";
   end Arch;

   --  arch64/arch.ads
   package Arch is
      type Target_Address is mod 2 ** 64;
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

   with Ada.Directories;       use Ada.Directories;
   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   with Ada.Text_IO;           use Ada.Text_IO;

   with GPR2.Options;
   with GPR2.Path_Name;
   with GPR2.Project.Tree;
   with GPR2.Project.View.Set;

   with Langkit_Support.Text;        use Langkit_Support.Text;
   with Libadalang.Analysis;         use Libadalang.Analysis;
   with Libadalang.Common;           use Libadalang.Common;
   with Libadalang.Project_Provider; use Libadalang.Project_Provider;

   procedure Run is

      procedure Iterate_Aggregated
        (Project  : GPR2.Project.View.Object;
         Callback : access procedure (Project : GPR2.Project.View.Object));
      --  If Project is not an aggregate project, just call Callback on it.
      --  Otherwise, recurse on Project's aggregated projects.

      procedure Process_Aggregated (Project : GPR2.Project.View.Object);
      --  Analyze all sources in Project

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      --  If Node is an object declaration, show the corresponding type
      --  declaration node.

      Tree : GPR2.Project.Tree.Object;

      ------------------------
      -- Iterate_Aggregated --
      ------------------------

      procedure Iterate_Aggregated
        (Project  : GPR2.Project.View.Object;
         Callback : access procedure (Project : GPR2.Project.View.Object))
      is
      begin
         if Project.Kind in GPR2.Aggregate_Kind then
            for A of Project.Aggregated loop
               Iterate_Aggregated (A, Callback);
            end loop;
         else
            Callback (Project);
         end if;
      end Iterate_Aggregated;

      ------------------------
      -- Process_Aggregated --
      ------------------------

      procedure Process_Aggregated (Project : GPR2.Project.View.Object) is
         --  Create an analysis context that has a view on all sources in Project

         UP       : constant Unit_Provider_Reference :=
           Create_Project_Unit_Provider (Tree, Project);
         Context  : constant Analysis_Context :=
            Create_Context (Unit_Provider => UP);
         Projects : GPR2.Project.View.Set.Object;
      begin
         Put_Line ("== Processing project: " & String (Project.Name) & " ==");

         Projects.Include (Project);
         for Filename of Source_Files (Tree, Default, Projects) loop
            declare
               F    : constant String := To_String (Filename);
               Unit : constant Analysis_Unit := Context.Get_From_File (F);
            begin
               Put_Line ("In " & Simple_Name (F) & ":");
               Unit.Root.Traverse (Process_Node'Access);
            end;
         end loop;

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
               Put_Line ("  Type for " & Node.Image
                         & ": " & Image (Type_Decl.Text));
            end;
         end if;

         return Into;
      end Process_Node;

      Opts : GPR2.Options.Object;
   begin
      --  Load the input project
      Opts.Add_Switch (GPR2.Options.P, "agg.gpr");
      if not Tree.Load
        (Opts,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error with "error loading the project tree";
      end if;

      --  Process every aggregated project independently
      Iterate_Aggregated (Tree.Root_Project, Process_Aggregated'Access);
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
