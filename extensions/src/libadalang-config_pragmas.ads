------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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

--  This package provides helpers to deal with GNAT's configurations pragmas
--  files.

with Ada.Containers.Hashed_Maps;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Config_Pragmas is

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Analysis_Unit,
      Element_Type    => Analysis_Unit,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Map analysis unit to the local configuration pragmas file that applies
   --  to it.

   type Config_Pragmas_Mapping is record
      Local_Pragmas : Unit_Maps.Map;
      --  Mappings that associate a local configuration pragmas file (element)
      --  to each analysis unit (key) for which it applies.

      Global_Pragmas : Analysis_Unit;
      --  Configuration pragmas file that applies to all analysis units, or
      --  ``No_Analysis_Unit`` if there is no such file.
   end record;

   procedure Set_Mapping
     (Context : Analysis_Context; Mapping : Config_Pragmas_Mapping);
   --  Assign in ``Context`` configuration pragmas files to analysis units as
   --  described in ``Mapping``.
   --
   --  This raises a ``Precondition_Failure`` exception if:
   --
   --  * ``Context`` is null;
   --  * any analysis unit in ``Mapping`` does not belong to ``Context``;
   --  * ``Local_Pragmas`` has a ``No_Analysis`` key or element.

   function Import_From_Project
     (Context    : Analysis_Context;
      Project    : Project_Tree;
      Subproject : Project_Type := No_Project) return Config_Pragmas_Mapping;
   --  Load configuration pragmas files referenced in ``Project`` and its
   --  sub-projects and return a mapping that describes for each analysis unit
   --  owned by ``Project`` which configuration pragmas files applies to it.
   --
   --  If ``Subproject`` is not ``No_Project``, restrict the exploration of
   --  local configuration pragmas files to that project (global ones are still
   --  found in the root project).

   procedure Import_From_Project
     (Context    : Analysis_Context;
      Project    : Project_Tree;
      Subproject : Project_Type := No_Project);
   --  Shortcut for ``Import_From_Project/Set_Mapping`` calls

end Libadalang.Config_Pragmas;
