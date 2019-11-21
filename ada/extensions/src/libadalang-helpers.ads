------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C)      2019, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Helpers is

   package Unit_Vectors is new
     Ada.Containers.Vectors (Positive, Analysis_Unit);

   --  This package is a convenient and easy way to create an application based
   --  on Libadalang, with out of the box facilities such as:
   --
   --  * Automatic handling of project files, including an option to process
   --    all the files of the project, and handling of scenario variables.
   --
   --  * Automatic command line option parser, with descriptive help.
   --
   --  As a user, all you have to do is to provide a ``Process_Unit``
   --  procedure, that will process one unit out of the set of units
   --  to process.
   generic
      Description : String;
      --  Description for the application. Will be used in the help string.

      with procedure Process_Context_Before
        (Ctx     : Analysis_Context;
         Project : GNATCOLL.Projects.Project_Tree_Access) is null;
      --  This procedure will be called right before going through units.  If a
      --  project was loaded, Project refers to it, otherwise it is null.

      with procedure Process_Unit
        (Unit : Analysis_Unit) is null;
      --  This procedure will be called once right after a unit is parsed

      with procedure Process_Context_After
        (Ctx     : Analysis_Context;
         Project : GNATCOLL.Projects.Project_Tree_Access;
         Units   : Unit_Vectors.Vector) is null;
      --  This procedure will be called once after all units have been parsed

   package App is

      --  This package contains the arguments parser for your app. It contains
      --  the instantiation of the parser, and the set of default arguments, so
      --  that you can also use their values in your app if needed.
      package Args is
         use GNATCOLL.Opt_Parse;

         Parser : Argument_Parser := Create_Argument_Parser
           (Help => Description);
         --  Argument parser for your application. Supports a set of default
         --  options. You can add your own on this parser.

         package Charset is new Parse_Option
           (Parser, "-C", "--charset", "Charset to use for source decoding",
            Unbounded_String,
            Default_Val => To_Unbounded_String ("iso-8859-1"));

         package Scenario_Vars is new Parse_Option_List
           (Parser, Short => "-X", Long => "--scenario-variable",
            Arg_Type      => Unbounded_String,
            Accumulate    => True,
            Help          => "Scenario variables to pass to the project file");

         package Project_File is new Parse_Option
           (Parser, "-P", "--project",
            Arg_Type    => Unbounded_String,
            Default_Val => Null_Unbounded_String,
            Help        => "Project file to use");

         package Files is new Parse_Positional_Arg_List
           (Parser,
            Name        => "files",
            Arg_Type    => Unbounded_String,
            Help        => "Files to analyze",
            Allow_Empty => True);

      end Args;

      procedure Run;
      --  Run the app. You should just call this from your main procedure for
      --  your project.
   end App;

end Libadalang.Helpers;
