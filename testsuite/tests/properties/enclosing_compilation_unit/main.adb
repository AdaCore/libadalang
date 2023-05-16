with Test;
procedure Main is
   -- This is to check which rebindings we associate with the enclosing
   -- compilation unit of an instantiated node.

   package My_Pkg is new Test;
   --% cu = node.p_designated_generic_decl.p_enclosing_compilation_unit
   --% cu.p_generic_instantiations
begin
   null;
   --% node.p_enclosing_compilation_unit
end Main;
