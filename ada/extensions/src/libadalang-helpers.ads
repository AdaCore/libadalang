with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;

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

      with procedure Process_Unit
        (Unit : Analysis_Unit) is null;
      --  This procedure will be called once right after a unit is parsed

      with procedure Process_Context
        (Ctx : Analysis_Context; Units : Unit_Vectors.Vector) is null;
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
