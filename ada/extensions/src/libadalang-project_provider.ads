------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

--  This package provides an ``Unit_Provider`` implementation that relies on a
--  project file.

package Libadalang.Project_Provider is

   use Support.Text;

   package LAL renames Libadalang.Analysis;
   package Prj renames GNATCOLL.Projects;

   type Project_Unit_Provider is new LAL.Unit_Provider_Interface with private;
   --  Unit provider backed up by a project file

   function Create_Project_Unit_Provider
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return Project_Unit_Provider
      with Pre => not Project.Root_Project.Is_Aggregate_Project;
   --  Create an unit provider using ``Project``. If ``Is_Project_Owner`` is
   --  true, the result owns ``Project``, thus the caller must not deallocate
   --  it itself. Otherwise, the project pointed by Project must outlive the
   --  returned unit file provider.
   --
   --  As unit providers must guarantee that there exists at most one source
   --  file for each couple (unit name, unit kind), aggregate projects are not
   --  supported.

   function Create_Project_Unit_Provider_Reference
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference;
   --  Wrapper around ``Create_Project_Unit_Provider`` as a shortcut to create
   --  a unit provider reference.
   --
   --% belongs-to: Project_Unit_Provider

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;
   --% no-document: True

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class;
   --% no-document: True

   function Convert
     (Kind : Analysis_Unit_Kind) return GNATCOLL.Projects.Unit_Parts
   is
     (case Kind is
      when Unit_Specification => GNATCOLL.Projects.Unit_Spec,
      when Unit_Body          => GNATCOLL.Projects.Unit_Body);
   --  Convert our kind for analysis unit into the corresponding
   --  ``GNATCOLL.Projects`` value.

private

   type Project_Unit_Provider is new LAL.Unit_Provider_Interface with record
      Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean;
   end record;

   overriding procedure Release (Provider : in out Project_Unit_Provider);

   function Create_Project_Unit_Provider
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True) return Project_Unit_Provider
   is ((Project          => Project,
        Env              => Env,
        Is_Project_Owner => Is_Project_Owner));

   function Create_Project_Unit_Provider_Reference
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return LAL.Unit_Provider_Reference
   is (LAL.Create_Unit_Provider_Reference
         (Create_Project_Unit_Provider (Project, Env, Is_Project_Owner)));

end Libadalang.Project_Provider;
