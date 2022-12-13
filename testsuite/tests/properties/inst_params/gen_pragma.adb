procedure Gen_Pragma is
   generic
      pragma Warnings (Off);
      type T is private;
      pragma Warnings (On);
   package Gen is
   end Gen;

   package My_Pkg is new Gen (Integer);
   --% node.p_inst_params
begin
   null;
end Gen_Pragma;

