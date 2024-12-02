--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Implementation; use Libadalang.Implementation;

private package Libadalang.Internal_Default_Provider is

   function Create return Internal_Unit_Provider_Access;
   --  Return an internal type for the default unit provider to use in
   --  Libadalang.

end Libadalang.Internal_Default_Provider;
