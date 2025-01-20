--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Unit_Files;

package body Libadalang.Internal_Default_Provider is

   ------------
   -- Create --
   ------------

   function Create return Internal_Unit_Provider_Access is
   begin
      return Wrap_Public_Provider (Unit_Files.Default_Provider);
   end Create;

end Libadalang.Internal_Default_Provider;
