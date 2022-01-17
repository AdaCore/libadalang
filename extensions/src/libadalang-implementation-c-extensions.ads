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

--  Extension to the generated C API for Libadalang-specific entry points

with Ada.Unchecked_Deallocation;

package Libadalang.Implementation.C.Extensions is

   type Project_Scenario_Variable is record
      Name, Value : chars_ptr;
   end record
      with Convention => C_Pass_By_Copy;

   type Project_Scenario_Variable_Array is
      array (Positive range <>) of Project_Scenario_Variable
      with Convention => C;

   function ada_create_project_unit_provider
     (Project_File, Project : chars_ptr;
      Scenario_Vars         : System.Address;
      Target, Runtime       : chars_ptr) return ada_unit_provider
      with Export     => True,
           Convention => C;

   function ada_create_auto_provider
     (Input_Files : System.Address;
      Charset     : chars_ptr) return ada_unit_provider
      with Export     => True,
           Convention => C;

   type Source_File_Array is array (int range <>) of chars_ptr;
   type Source_File_Array_Ref (Length : int) is record
      C_Ptr : System.Address;
      --  Pointer to the first source file (i.e. pointer on the file array), to
      --  access elements from the C API.

      Items : Source_File_Array (1 .. Length);
   end record;
   type Source_File_Array_Ref_Access is access all Source_File_Array_Ref;

   procedure Free is new Ada.Unchecked_Deallocation
     (Source_File_Array_Ref, Source_File_Array_Ref_Access);

   function ada_project_source_files
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr;
      Mode            : int) return Source_File_Array_Ref_Access
     with Export, Convention => C;
   --  Load the project file according to ``Project_File``, ``Scenario_Vars``,
   --  ``Target`` and ``Runtime``. On success, compute the list of source files
   --  in this project according to ``Mode`` (whose value maps to positions in
   --  the ``Libadalang.Project_Provider.Source_Files_Mode`` enum) and return
   --  it.
   --
   --  On project loading failure, return null and set the exception info
   --  accordingly.

   procedure ada_free_source_file_array
     (Source_Files : Source_File_Array_Ref_Access)
     with Export, Convention => C;
   --  Free the given list of source files

end Libadalang.Implementation.C.Extensions;
