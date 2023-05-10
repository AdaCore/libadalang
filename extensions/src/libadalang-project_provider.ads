--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Projects;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Vector;
with GPR2.Project.View.Set;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

--  This package provides an ``Unit_Provider`` implementation that relies on a
--  project file.

package Libadalang.Project_Provider is

   use Support.Text;

   package LAL renames Libadalang.Analysis;
   package Prj renames GNATCOLL.Projects;

   Trace : constant GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LIBADALANG.PROJECT_PROVIDER", GNATCOLL.Traces.From_Config);

   Unsupported_View_Error : exception;
   --  See the ``Create_Project_Unit_Provider`` functions below

   ---------------------------------
   -- GNATCOLL.Projects based API --
   ---------------------------------

   type Provider_And_Projects is record
      Provider : LAL.Unit_Provider_Reference;
      Projects : Prj.Project_Array_Access;
   end record;
   --  Associates one project unit provider with all the projects on which it
   --  has visibility.

   type Provider_And_Projects_Array is
      array (Positive range <>) of Provider_And_Projects;
   type Provider_And_Projects_Array_Access is
      access all Provider_And_Projects_Array;
   procedure Free (PAP_Array : in out Provider_And_Projects_Array_Access);

   function Create_Project_Unit_Providers
     (Tree : Prj.Project_Tree_Access)
      return Provider_And_Projects_Array_Access;
   --  Create unit providers for consistent sets of projects in ``Tree``.
   --
   --  As unit providers must guarantee that there exists at most one source
   --  file for each couple (unit name, unit kind), this creates more than one
   --  unit providers when Project is an aggregate project that contains
   --  multiple definitions for the same unit.
   --
   --  The project pointed to by ``Tree`` must outlive the returned unit file
   --  providers, and it is up to callers to deallocate ``Tree`` itself.

   function Create_Project_Unit_Provider
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type := Prj.No_Project;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference;
   --  Likewise, but create only one unit provider.
   --
   --  If a non-null ``Project`` is given, use it to provide units. Raise an
   --  ``Unsupported_View_Error`` exception if that project aggregates more
   --  than one project in its closure.
   --
   --  If Project is not provided, run ``Create_Project_Unit_Providers``: if it
   --  returns only one provider, return it, otherwise raise an
   --  ``Unsupported_View_Error`` exception.
   --
   --  If ``Is_Project_Owner`` is true, the result owns ``Tree``, thus the
   --  caller must not deallocate it itself.  Otherwise, the project pointed to
   --  by ``Project`` must outlive the returned unit file provider.

   function Convert
     (Kind : Analysis_Unit_Kind) return GNATCOLL.Projects.Unit_Parts
   is
     (case Kind is
      when Unit_Specification => GNATCOLL.Projects.Unit_Spec,
      when Unit_Body          => GNATCOLL.Projects.Unit_Body);
   --  Convert our kind for analysis unit into the corresponding
   --  ``GNATCOLL.Projects`` value.

   package Filename_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");

   type Source_Files_Mode is
     (Default, Root_Project, Whole_Project, Whole_Project_With_Runtime);

   function Source_Files
     (Tree     : Prj.Project_Tree'Class;
      Mode     : Source_Files_Mode := Default;
      Projects : Prj.Project_Array := Prj.Empty_Project_Array)
      return Filename_Vectors.Vector;
   --  Return the list of source files in the given project ``Tree``. Which
   --  sources are considered depends on ``Mode``:
   --
   --  * ``Default``: sources in the root project and its non-externally built
   --    dependencies;
   --
   --  * ``Root_Project``: sources in the root project only;
   --
   --  * ``Whole_Project``: sources in the whole project tree (i.e. including
   --    externally built dependencies);
   --
   --  * ``Whole_Project_With_Runtime``: sources in the whole project tree plus
   --    runtime sources.
   --
   --  If ``Projects`` is not empty, return instead the list for the sources in
   --  all sub-projects in ``Projects``, still applying the given mode to the
   --  search.

   function Default_Charset_From_Project
     (Tree    : Prj.Project_Tree'Class;
      Project : Prj.Project_Type := Prj.No_Project) return String;
   --  Try to detect the default charset to use for the given project.
   --
   --  Restrict the detection to the subproject ``Project``, or to ``Tree``'s
   --  root project if left to ``Prj.No_Project``.
   --
   --  Note that, as of today, this detection only looks for the ``-gnatW8``
   --  compiler switch: other charsets are not supported.

   --------------------
   -- GPR2 based API --
   --------------------

   --  .. ATTENTION:: This is an experimental feature, so even if it is exposed
   --  to allow experiments, it is totally unsupported and the API is very
   --  likely to change in the future.

   type GPR2_Provider_And_Projects is record
      Provider : LAL.Unit_Provider_Reference;
      Projects : GPR2.Project.View.Vector.Object;
   end record;
   --  Associates one project unit provider with all the projects on which it
   --  has visibility.

   type GPR2_Provider_And_Projects_Array is
      array (Positive range <>) of GPR2_Provider_And_Projects;
   type GPR2_Provider_And_Projects_Array_Access is
      access all GPR2_Provider_And_Projects_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (GPR2_Provider_And_Projects_Array,
      GPR2_Provider_And_Projects_Array_Access);

   function Create_Project_Unit_Providers
     (Tree : GPR2.Project.Tree.Object)
      return GPR2_Provider_And_Projects_Array_Access;
   --  Create unit providers for consistent sets of projects in ``Tree``.
   --
   --  As unit providers must guarantee that there exists at most one source
   --  file for each couple (unit name, unit kind), this creates more than one
   --  unit providers when Project is an aggregate project that contains
   --  multiple definitions for the same unit.
   --
   --  The project pointed to by ``Tree`` must outlive the returned unit file
   --  providers, and it is up to callers to deallocate ``Tree`` itself.

   function Create_Project_Unit_Provider
     (Tree             : GPR2.Project.Tree.Object;
      Project          : GPR2.Project.View.Object :=
                           GPR2.Project.View.Undefined)
      return LAL.Unit_Provider_Reference;
   --  Likewise, but create only one unit provider.
   --
   --  If a non-null ``Project`` is given, use it to provide units. Raise an
   --  ``Unsupported_View_Error`` exception if that project aggregates more
   --  than one project in its closure.
   --
   --  If Project is not provided, run ``Create_Project_Unit_Providers``: if it
   --  returns only one provider, return it, otherwise raise an
   --  ``Unsupported_View_Error`` exception.

   function Source_Files
     (Tree     : GPR2.Project.Tree.Object;
      Mode     : Source_Files_Mode := Default;
      Projects : GPR2.Project.View.Set.Object := GPR2.Project.View.Set.Empty)
      return Filename_Vectors.Vector;
   --  Return the list of source files in the given project ``Tree``. Which
   --  sources are considered depends on ``Mode``:
   --
   --  * ``Default``: sources in the root project and its non-externally built
   --    dependencies;
   --
   --  * ``Root_Project``: sources in the root project only;
   --
   --  * ``Whole_Project``: sources in the whole project tree (i.e. including
   --    externally built dependencies);
   --
   --  * ``Whole_Project_With_Runtime``: sources in the whole project tree plus
   --    runtime sources.
   --
   --  If ``Projects`` is not empty, return instead the list for the sources in
   --  all sub-projects in ``Projects``, still applying the given mode to the
   --  search.

   function Default_Charset_From_Project
     (Tree    : GPR2.Project.Tree.Object;
      Project : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return String;
   --  Try to detect the default charset to use for the given project.
   --
   --  Restrict the detection to the subproject ``Project``, or to ``Tree``'s
   --  root project if left to ``Prj.No_Project``.
   --
   --  Note that, as of today, this detection only looks for the ``-gnatW8``
   --  compiler switch: other charsets are not supported.

end Libadalang.Project_Provider;
