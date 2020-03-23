.. _examples_aggregate_projects:

Dealing with aggregate projects
###############################

Processing aggregate projects with Libadalang is peculiar. Please get familiar
with the :ref:`unit-providers-aggregate` section before reading this.

Input source
============

Get the projects files and sources from the :ref:`unit-providers-aggregate`
section.

Sample code
===========

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
