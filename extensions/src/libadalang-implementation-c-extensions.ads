--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Extension to the generated C API for Libadalang-specific entry points

with Ada.Unchecked_Deallocation;

with GNATCOLL.Projects; use GNATCOLL.Projects;

package Libadalang.Implementation.C.Extensions is

   ----------------------
   -- Project handling --
   ----------------------

   type ada_gpr_project is record
      Tree : Project_Tree_Access;
      Env  : Project_Environment_Access;
   end record
      with Convention => C;

   type ada_gpr_project_ptr is access all ada_gpr_project
      with Convention => C;

   procedure Free is new Ada.Unchecked_Deallocation
     (ada_gpr_project, ada_gpr_project_ptr);

   type ada_gpr_project_scenario_variable is record
      Name, Value : chars_ptr;
   end record
      with Convention => C_Pass_By_Copy;

   type ada_gpr_project_scenario_variable_array is
      array (Positive range <>) of ada_gpr_project_scenario_variable
      with Convention => C;
   --  Array of name/value definitions for scenario variables. The last entry
   --  in such arrays must be a null/null association.

   function ada_gpr_project_load
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr) return ada_gpr_project_ptr
     with Export, Convention => C;
   --  Load a project file with the given parameter. On success, allocate and
   --  return an ``ada_gpr_project`` record. Raise an ``Invalid_Project``
   --  exception on failure.

   procedure ada_gpr_project_free (Self : ada_gpr_project_ptr)
     with Export, Convention => C;
   --  Free resources allocated for ``Self``

   function ada_gpr_project_create_unit_provider
     (Self    : ada_gpr_project_ptr;
      Project : chars_ptr) return ada_unit_provider
     with Export, Convention => C;
   --  Create a project provider using the given GPR project. If ``Project`` is
   --  passed, it must be the name of a sub-project. If the selected project
   --  contains conflicting sources, raise an ``Inavlid_Project`` exception.

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

   function ada_gpr_project_source_files
     (Self : ada_gpr_project_ptr;
      Mode : int) return Source_File_Array_Ref_Access
     with Export, Convention => C;
   --  Compute the list of source files in the given GPR project according to
   --  ``Mode`` (whose value maps to positions in the
   --  ``Libadalang.Project_Provider.Source_Files_Mode`` enum) and return it.

   procedure ada_gpr_project_free_source_files
     (Source_Files : Source_File_Array_Ref_Access)
     with Export, Convention => C;
   --  Free the given list of source files

   function ada_create_project_unit_provider
     (Project_File, Project : chars_ptr;
      Scenario_Vars         : System.Address;
      Target, Runtime       : chars_ptr) return ada_unit_provider
     with Export, Convention => C;
   --  Load a project file and create a unit provider for it in one pass

   ------------------------
   -- Auto unit provider --
   ------------------------

   function ada_create_auto_provider
     (Input_Files : System.Address;
      Charset     : chars_ptr) return ada_unit_provider
      with Export     => True,
           Convention => C;

   ------------------
   -- Preprocessor --
   ------------------

   function ada_create_preprocessor_from_file
     (Filename    : chars_ptr;
      Path_Data   : access chars_ptr;
      Path_Length : int;
      Line_Mode   : access int) return ada_file_reader
   with Export => True, Convention => C;
   --  Load the preprocessor data file at ``Filename`` using, directory names
   --  in the ``Path_Data``/``Path_Length`` array  to look for definition
   --  files. If ``Line_Mode`` is not null, use it to force the line mode in
   --  each preprocessed source file. Return a file reader that preprocesses
   --  sources accordingly.

   function ada_gpr_project_create_preprocessor
     (Self    : ada_gpr_project_ptr;
      Project : chars_ptr) return ada_file_reader
   with Export, Convention => C;
   --  Create preprocessor data from compiler arguments found in the given GPR
   --  project (``-gnateP`` and ``-gnateD`` arguments), or from the ``Project``
   --  sub-project (if the argument is passed).
   --
   --  Note that this function collects all arguments and returns an
   --  approximation from them: it does not replicates exactly gprbuild's
   --  behavior. This may raise a ``File_Read_Error`` exception if this fails
   --  to read a preprocessor data file and a ``Syntax_Error`` exception if one
   --  such file has invalid syntax.

end Libadalang.Implementation.C.Extensions;
