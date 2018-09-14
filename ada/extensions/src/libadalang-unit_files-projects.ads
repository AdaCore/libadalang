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
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.   See the  GNU  --
-- General  Public  License for more details.   You should have received a  --
-- copy of the GNU General Public License  distributed with this software;  --
-- see  file  COPYING3.  If not, go to  http://www.gnu.org/licenses  for a  --
-- complete copy of the license.                                            --
------------------------------------------------------------------------------

with GNATCOLL.Projects;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

--  This package provides an Unit_Provider implemetation that relies on a
--  project file.

package Libadalang.Unit_Files.Projects is

   package LAL renames Libadalang.Analysis;

   type Project_Unit_Provider is new LAL.Unit_Provider_Interface with private;
   --  Unit_Provider implementation that relies on a project file

   package Prj renames GNATCOLL.Projects;

   function Create_Project_Unit_Provider
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean)
      return Project_Unit_Provider;
   --  Create an unit provider using Project. If Is_Project_Owner is true,
   --  the result owns Project, thus the caller must not deallocate it itself.
   --  Otherwise, the project pointed by Project must outlive the returned unit
   --  file provider.

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

   function Convert
     (Kind : Analysis_Unit_Kind) return GNATCOLL.Projects.Unit_Parts
   is
     (case Kind is
      when Unit_Specification => GNATCOLL.Projects.Unit_Spec,
      when Unit_Body          => GNATCOLL.Projects.Unit_Body);

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
      Is_Project_Owner : Boolean) return Project_Unit_Provider
   is ((Project          => Project,
        Env              => Env,
        Is_Project_Owner => Is_Project_Owner));

end Libadalang.Unit_Files.Projects;
