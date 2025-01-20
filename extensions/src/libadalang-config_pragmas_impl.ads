--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
