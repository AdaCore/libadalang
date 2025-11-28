--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Implementation;

package body Libadalang.Config_Pragmas_Impl is

   package Impl renames Libadalang.Implementation;

   -------------------------
   -- Config_Pragmas_Unit --
   -------------------------

   function Config_Pragmas_Unit
     (Context             : Internal_Context;
      Config_Pragmas_File : Config_Pragmas_File_Access) return Internal_Unit is
   begin
      if Config_Pragmas_File = null then
         return null;
      end if;

      if Config_Pragmas_File.Unit = null then
         Config_Pragmas_File.Unit :=
           Internal_Unit
             (Impl.Get_From_File
                (Context  => Impl.Internal_Context (Context),
                 Filename => +Config_Pragmas_File.Filename.Full_Name,
                 Charset  => "",
                 Reparse  => False,
                 Rule     => Default_Grammar_Rule));
      end if;

      return Config_Pragmas_File.Unit;
   end Config_Pragmas_Unit;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Internal_Config_Pragmas_Mapping) is
   begin
      for E of Self.Entries loop
         Free (E);
      end loop;
      Self.Entries.Clear;
      Self.Local_Pragmas.Clear;
      Self.Global_Pragmas := null;
   end Free;

end Libadalang.Config_Pragmas_Impl;
