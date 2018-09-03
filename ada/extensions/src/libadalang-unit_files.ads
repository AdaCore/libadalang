------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

private with GNATCOLL.Locks;

package Libadalang.Unit_Files is
private

   use GNATCOLL.Locks;

   GPR_Lock : aliased Mutual_Exclusion;
   --  Lock to serialize all calls to GNATCOLL.Projects, which is not
   --  thread-safe. Hopefully we will be able to get rid of this lock when
   --  moving to Libgpr2.

end Libadalang.Unit_Files;
