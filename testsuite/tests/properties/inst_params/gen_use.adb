procedure Gen_Use is
   generic
      type T is private;
      use type T;
   package Gen is
   end Gen;

   package My_Pkg is new Gen (Integer);
   --% node.p_inst_params
begin
   null;
end Gen_Use;
