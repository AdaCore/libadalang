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

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

package Libadalang.Unit_Files is

   use Support.Text;

   package LAL renames Libadalang.Analysis;

   function Default_Provider return LAL.Unit_Provider_Reference;
   --  Default implementation for the Unit_Provider mechanism. It assumes that
   --  each compilation unit gets its own source file in the current directory,
   --  named according to the GNAT convention: See
   --  <http://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn
   --  /the_gnat_compilation_model.html#file-naming-rules> for more details. It
   --  also assumes these sources use GNAT's default native runtime.

   function Unit_String_Name (Name : Text_Type) return String;
   --  Return Name after lowering its case and encoding it appropriately for
   --  the file system.

   function File_From_Unit
     (Name : Text_Type; Kind : Analysis_Unit_Kind) return String;
   --  Convert an unit name and unit kind into the default filename

end Libadalang.Unit_Files;
