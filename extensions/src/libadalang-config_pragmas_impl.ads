--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides implementation helpers to deal with GNAT's
--  configurations pragmas files.

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS; use GNATCOLL.VFS;

limited with Libadalang.Implementation;

private package Libadalang.Config_Pragmas_Impl is

   type Internal_Context is access all Implementation.Analysis_Context_Type;
   type Internal_Unit is access all Implementation.Analysis_Unit_Type;

   type Config_Pragmas_File_Record is record
      Filename : Virtual_File;
      Unit     : Internal_Unit;
   end record;
   --  Configuration pragma file.
   --
   --  Initially, only its filename is known (``Unit`` is null). When the
   --  actual config pragmas are requested, the filename is parsed, and
   --  ``Unit`` is set to the corresponding analysis unit.

   type Config_Pragmas_File_Access is access all Config_Pragmas_File_Record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Config_Pragmas_File_Record, Config_Pragmas_File_Access);

   function Config_Pragmas_Unit
     (Context            : Internal_Context;
      Config_Pragmas_File : Config_Pragmas_File_Access) return Internal_Unit;
   --  Return the analysis unit that contains the configuration pragmas for
   --  ``Config_Pragmas_File`` (parse it if needed).

   package Config_Pragmas_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Config_Pragmas_File_Access,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=");
   --  Map filename of analysis unit to the local configuration pragmas file
   --  that applies to it.

   package Entry_Vectors is new Ada.Containers.Vectors
     (Positive, Config_Pragmas_File_Access);

   type Internal_Config_Pragmas_Mapping is record
      Entries : Entry_Vectors.Vector;
      --  List of Config_Pragmas_File_Record objects allocated for this
      --  mapping.  Used to reclaim the corresponding memory.

      Local_Pragmas : Config_Pragmas_File_Maps.Map;
      --  Mappings that associate a local configuration pragmas file (element)
      --  to each analysis unit (key) for which it applies.

      Global_Pragmas : Config_Pragmas_File_Access;
      --  Configuration pragmas file that applies to all analysis units
   end record;

   procedure Free (Self : in out Internal_Config_Pragmas_Mapping);
   --  Free all resources allocated for ``Self``

end Libadalang.Config_Pragmas_Impl;
