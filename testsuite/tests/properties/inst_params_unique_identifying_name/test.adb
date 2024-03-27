procedure Test is
   generic
      X : Integer;
   package Gen is
   end Gen;

   package Inst is new Gen (0);
   --% params = node.p_inst_params
   --% params[0].param.p_unique_identifying_name
begin
   null;
end Test;
