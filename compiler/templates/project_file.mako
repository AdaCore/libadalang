with "liblang_support";

library project ${lib_name} is

   type Library_Kind_Type is ("static", "relocatable");
   Library_Kind_Param : Library_Kind_Type :=
     external ("LIBRARY_TYPE", "relocatable");

   for Languages use ("Ada", "C");
   for Library_Name use "${capi.shared_object_basename}";
   for Library_Kind use Library_Kind_Param;
   for Interfaces use
     ("libadalang.adb",
      "libadalang.ads",
      "libadalang-c.adb",
      "libadalang-c.ads",
      "libadalang-lexer.adb",
      "libadalang-lexer.ads",
      "libadalang-parsers.adb",
      "libadalang-parsers.ads",
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

   package Compiler is
      for Default_Switches ("C") use
        ("-I${quex_path}", "-Wno-deprecated-register",
         "-DQUEX_OPTION_ASSERTS_DISABLED",
         "-DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED");
   end Compiler;

end ${lib_name};
