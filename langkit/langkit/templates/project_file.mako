with "gnatcoll";
with "gnatcoll_iconv";
with "langkit_support";

library project ${lib_name} is

   type Build_Mode_Type is ("dev", "prod");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "dev");

   type Library_Kind_Type is ("static", "relocatable");
   Library_Kind_Param : Library_Kind_Type :=
     external ("LIBRARY_TYPE", "relocatable");

   type Boolean is ("false", "true");
   Is_Externally_Built : Boolean :=
     external ("${lib_name.upper()}_EXTERNALLY_BUILT", "true");

   for Externally_Built use Is_Externally_Built;

   for Languages use ("Ada", "C");
   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;
   for Interfaces use
     ("${lib_name.lower()}.adb",
      "${lib_name.lower()}.ads",
      "${lib_name.lower()}-c.adb",
      "${lib_name.lower()}-c.ads",
      "${lib_name.lower()}-lexer.adb",
      "${lib_name.lower()}-lexer.ads",
      "${lib_name.lower()}-parsers.adb",
      "${lib_name.lower()}-parsers.ads",
      "quex_interface.c",
      "quex_interface.h",
      "quex_lexer.c",
      "quex_lexer-configuration.h",
      "quex_lexer.h",
      "quex_lexer-token.h",
      "quex_lexer-token_ids.h");

   for Source_Dirs use ("../../include/${lib_name.lower()}");
   for Library_Dir use "../";
   for Library_ALI_Dir
      use "../${lib_name.lower()}/" & Library_Kind_Param;
   for Object_Dir
      use "../../obj/${lib_name.lower()}/" & Library_Kind_Param;

   Common_C_Cargs :=
     ("-I${quex_path}",
      "-DQUEX_OPTION_ASSERTS_DISABLED",
      "-DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED");

   package Compiler is
      case Build_Mode is
         when "dev" =>
            for Default_Switches ("Ada") use ("-g", "-O0");
            for Default_Switches ("C") use Common_C_Cargs & ("-g3", "-O0");

            for Switches ("quex_lexer.c") use Common_C_Cargs & ("-g0", "-O0");
            --  This file is *huge* and the debugging information for it harms
            --  Valgrind runs. We almost never have to debug this file so
            --  this is acceptable.

         when "prod" =>
            --  Debug information is useful even with optimization for
            --  profiling, for instance.
            for Default_Switches ("Ada") use ("-g", "-Ofast", "-gnatp");
            for Default_Switches ("C") use Common_C_Cargs
              & ("-Ofast",
                 -- Deactivate because of memory usage, see OA26-005
                 "-fno-ree");
      end case;
   end Compiler;

end ${lib_name};
