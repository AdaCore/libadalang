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

with GNATCOLL.Projects;
with GNATCOLL.Traces; use GNATCOLL.Traces;

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
   --  The project pointed to by ``Tree`` and ``Env`` must outlive the returned
   --  unit file providers, and it is up to callers must deallocate
   --  ``Tree``/``Env`` themselves.

   function Create_Project_Unit_Provider
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type := Prj.No_Project;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference;
   --  Likewise, but create only one unit provider.
   --
   --  If a non-null Project is given, use it to provide units. Raise an
   --  Invalid_Project exception if an aggregate projects that aggregates more
   --  than one project is in its closure.
   --
   --  If Project is not provided, run Create_Project_Unit_Providers: if it
   --  returns only one provider, return it, otherwise raise an error.
   --
   --  If ``Is_Project_Owner`` is true, the result owns ``Tree``, thus the
   --  caller must not deallocate it itself.  Otherwise, the project pointed to
   --  by Project must outlive the returned unit file provider.

   function Convert
     (Kind : Analysis_Unit_Kind) return GNATCOLL.Projects.Unit_Parts
   is
     (case Kind is
      when Unit_Specification => GNATCOLL.Projects.Unit_Spec,
      when Unit_Body          => GNATCOLL.Projects.Unit_Body);
   --  Convert our kind for analysis unit into the corresponding
   --  ``GNATCOLL.Projects`` value.

end Libadalang.Project_Provider;
