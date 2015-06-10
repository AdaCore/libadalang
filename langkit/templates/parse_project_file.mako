with "langkit_support";
with "${lib_name.lower()}";

project Parse is

   for Languages use ("Ada");

   for Source_Dirs use (".");
   for Exec_Dir use "../bin";
   for Object_Dir use "../obj/parse";

   for Main use ("parse.adb");

end Parse;
