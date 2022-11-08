--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Helper for ``Libadalang.Preprocessing``, implementing preprocessor data
--  extraction from GPR files.

with GNATCOLL.Projects;

with Libadalang.Preprocessing; use Libadalang.Preprocessing;

private package Libadalang.PP_GPR is

   package Prj renames GNATCOLL.Projects;

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Prj.Project_Tree'Class;
      Project        : Prj.Project_Type := Prj.No_Project;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map);
   --  Create preprocessor data from compiler arguments found in the given GPR
   --  project (``-gnateP`` and ``-gnateD`` arguments).
   --
   --  If a non-null ``Project`` is given, look for compiler arguments in it
   --  and the other projects in its closure.  If ``Project`` is left to
   --  ``No_Project``, try to use the whole project tree.
   --
   --  Note that this function collects all arguments and returns an
   --  approximation from them: it does not replicates exactly gprbuild's
   --  behavior.

end Libadalang.PP_GPR;
