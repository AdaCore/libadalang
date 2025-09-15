--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Extension to the generated C API for Libadalang-specific entry points

with Ada.Unchecked_Deallocation;

package Libadalang.Implementation.C.Extensions is

   type C_String_Array is array (int range <>) of chars_ptr;
   type ada_string_array (Length : int) is record
      C_Ptr : System.Address;
      --  Pointer to the first string (i.e. pointer on the array), to access
      --  elements from the C API.

      Items : C_String_Array (1 .. Length);
   end record;
   type ada_string_array_ptr is access all ada_string_array;

   procedure Free is new Ada.Unchecked_Deallocation
     (ada_string_array, ada_string_array_ptr);

   procedure ada_free_string_array (Strings : ada_string_array_ptr)
     with Export, Convention => C;
   --  Free the given list of source files

   ----------------------
   -- Project handling --
   ----------------------

   type ada_gpr_options is new System.Address;
   --  Holder for GPR loading options

   function ada_gpr_options_create return ada_gpr_options
   with Export, Convention => C;
   --  Create a new holder for GPR loading options

   procedure ada_gpr_options_add_switch
     (Options  : ada_gpr_options;
      Switch   : int;
      Param    : chars_ptr;
      Index    : chars_ptr;
      Override : int)
   with Export, Convention => C;
   --  Add a switch to the given options.
   --
   --  ``Switch`` is the position of the ``GPR2.Options.Option`` enumeration
   --  value for the switch to add.
   --
   --  ``Param`` and ``Index`` are the strings to pass to
   --  ``GPR2.Options.Add_Switch``, and can be null to designate empty strings.
   --
   --  ``Override`` encodes False as zero and True as any other value.

   procedure ada_gpr_options_free (Options : ada_gpr_options)
   with Export, Convention => C;
   --  Dellocate the given holder for GPR loading options

   type ada_gpr_project is new System.Address;

   procedure ada_gpr_project_load
     (Options  : ada_gpr_options;
      Ada_Only : int;
      Project  : access ada_gpr_project;
      Errors   : access ada_string_array_ptr)
     with Export, Convention => C;
   --  Load the ``Project_File`` GPR file with the given options.
   --
   --  If ``Ada_Only`` is true, call ``Restrict_Autoconf_To_Languages`` to
   --  make GPR only consider the Ada language.
   --
   --  On success, set ``Project`` to a newly allocated ``ada_gpr_project``, as
   --  well as a possibly empty list of error messages in ``Errors``.  Raise an
   --  ``Invalid_Project`` exception on failure.

   procedure ada_gpr_project_free (Self : ada_gpr_project)
     with Export, Convention => C;
   --  Free resources allocated for ``Self``

   function ada_gpr_project_create_unit_provider
     (Self    : ada_gpr_project;
      Project : chars_ptr) return ada_unit_provider
     with Export, Convention => C;
   --  Create a project provider using the given GPR project ``Self``.
   --
   --  If ``Project`` is passed, it must be the name of a sub-project. If the
   --  selected project contains conflicting sources, raise an
   --  ``Inavlid_Project`` exception.
   --
   --  The returned unit provider assumes that resources allocated by ``Self``
   --  are kept live: it is the responsibility of the caller to make
   --  ``Self`` live at least as long as the returned unit provider.

   function ada_gpr_project_source_files
     (Self            : ada_gpr_project;
      Mode            : int;
      Projects_Data   : access chars_ptr;
      Projects_Length : int) return ada_string_array_ptr
     with Export, Convention => C;
   --  Compute the list of source files in the given GPR project according to
   --  ``Mode`` (whose value maps to positions in the
   --  ``Libadalang.Project_Provider.Source_Files_Mode`` enum) and return it.
   --
   --  If the string array designated by ``Projects_Data``/``Projects_Length``
   --  is not empty, return instead the list for the sources in all
   --  sub-projects in ``Projects``, still applying the given mode to the
   --  search.

   function ada_gpr_project_default_charset
     (Self : ada_gpr_project; Project : chars_ptr) return chars_ptr
   with Export, Convention => C;
   --  Try to detect the default charset to use for the given project.
   --
   --  Restrict the detection to the subproject ``Project``, or to ``Self``'s
   --  root project if left to ``Prj.No_Project``.
   --
   --  Note that, as of today, this detection only looks for the ``-gnatW8``
   --  compiler switch: other charsets are not supported.

   procedure ada_gpr_project_initialize_context
     (Self          : ada_gpr_project;
      Context       : ada_analysis_context;
      Project       : chars_ptr;
      Event_Handler : ada_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int)
     with Export, Convention => C;
   --  Wrapper around ``Initialize_Context_From_Project`` to initialize
   --  ``Context`` (an already allocated but not yet initialized analysis
   --  context) from ``Self``.

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
     (Self      : ada_gpr_project;
      Project   : chars_ptr;
      Line_Mode : access int) return ada_file_reader
   with Export, Convention => C;
   --  Create preprocessor data from compiler arguments found in the given GPR
   --  project ``Self`` (``-gnatep`` and ``-gnateD`` compiler switches), or
   --  from the ``Project`` sub-project (if the argument is passed).
   --
   --  If ``Line_Mode`` is not null, use it to force the line mode in each
   --  preprocessed source file.
   --
   --  Note that this function collects all arguments and returns an
   --  approximation from them: it does not replicates exactly gprbuild's
   --  behavior. This may raise a ``File_Read_Error`` exception if this fails
   --  to read a preprocessor data file and a ``Syntax_Error`` exception if one
   --  such file has invalid syntax.
   --
   --  The returned file reader assumes that resources allocated by ``Self``
   --  are kept live: it is the responsibility of the caller to make ``Self``
   --  live at least as long as the returned file reader.

   --------------------
   -- Config pragmas --
   --------------------

   procedure ada_set_config_pragmas_mapping
     (Context        : ada_analysis_context;
      Global_Pragmas : ada_analysis_unit;
      Local_Pragmas  : access ada_analysis_unit)
     with Export, Convention => C;
   --  See the C header

   ------------------------
   -- Target information --
   ------------------------

   type ada_target_information is access Target_Information;
   procedure Free is new Ada.Unchecked_Deallocation
     (Target_Information, ada_target_information);

   function ada_target_info_load
     (Filename : chars_ptr) return ada_target_information
     with Export, Convention => C;
   --  Read target information from `Filename` and return it. Raise an
   --  `Langkit_Support.Errors.Invalid_Input` exception if it cannot be read of
   --  if there is any trouble decoding it.

   procedure ada_target_info_free (Self : ada_target_information)
     with Export, Convention => C;
   --  Free resources allocated for `Self`

   procedure ada_target_info_set
     (Self : ada_analysis_context; Info : ada_target_information)
     with Export, Convention => C;
   --  Assign target information to ``self``. This will affect specific bound
   --  values for types defined in the built in Standard package.

end Libadalang.Implementation.C.Extensions;
