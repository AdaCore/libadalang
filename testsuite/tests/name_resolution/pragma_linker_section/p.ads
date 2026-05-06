package P is
   subtype T_1 is Integer;
   pragma Linker_Section (T_1, ".my_section");
   pragma Test_Statement;
end P;
