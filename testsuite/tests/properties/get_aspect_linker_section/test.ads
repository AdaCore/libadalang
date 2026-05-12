package Test is
   subtype S_1 is String;
   --% node.p_get_aspect("Linker_Section")
   subtype S_2 is String with Linker_Section => (".my_section");
   --% node.p_get_aspect("Linker_Section")
   subtype S_3 is String;
   --% node.p_get_aspect("Linker_Section")

   pragma Linker_Section (S_1, ".my_section");

   subtype S_S_1 is S_1;
   --% node.p_get_aspect("Linker_Section")
   type D_S_1 is new S_1;
   --% node.p_get_aspect("Linker_Section")

end Test;
