procedure Test is
   package Pkg is
      procedure Hello;
   end Pkg;

   package body Pkg is
      procedure Hello is null;
   end Pkg;
begin
   Pkg.Hello (2);
   --% node.f_call.f_name.p_referenced_decl()
   --% node.f_call.f_name.p_referenced_decl(imprecise_fallback=True)
end Test;
