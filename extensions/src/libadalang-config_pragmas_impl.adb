--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Implementation;

package body Libadalang.Config_Pragmas_Impl is

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Internal_Unit) return Hash_Type is
   begin
      return Implementation.Hash (Implementation.Internal_Unit (Unit));
   end Hash;

end Libadalang.Config_Pragmas_Impl;
