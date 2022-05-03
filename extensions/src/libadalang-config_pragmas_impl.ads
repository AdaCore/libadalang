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

--  This package provides implementation helpers to deal with GNAT's
--  configurations pragmas files.

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

limited with Libadalang.Implementation;

private package Libadalang.Config_Pragmas_Impl is

   type Internal_Unit is access all Implementation.Analysis_Unit_Type;

   function Hash (Unit : Internal_Unit) return Hash_Type;

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Internal_Unit,
      Element_Type    => Internal_Unit,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Map analysis unit to the local configuration pragmas file that applies
   --  to it.

   type Internal_Config_Pragmas_Mapping is record
      Local_Pragmas : Unit_Maps.Map;
      --  Mappings that associate a local configuration pragmas file (element)
      --  to each analysis unit (key) for which it applies.

      Global_Pragmas : Internal_Unit;
      --  Configuration pragmas file that applies to all analysis units
   end record;

end Libadalang.Config_Pragmas_Impl;
