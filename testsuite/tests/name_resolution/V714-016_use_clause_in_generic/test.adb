procedure Main is
   generic
   package Config is
   end Config;

   generic
      with package G is new Config;
   package Impl is
      package R renames G;
      use R;
   end Impl;

   package My_Config is new Config;
   package My_Inst is new Impl (My_Config);
   --% node.p_designated_generic_decl[1][2][0][0].p_renamed_package
begin
   null;
end Main;

