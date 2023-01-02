with Pkg;

procedure Test is
   package My_Pkg is new Pkg;
   --% gen_decl=node.p_designated_generic_decl
   --% gen_body=gen_decl.p_next_part_for_decl()
   --% usage=gen_body.find(lal.DottedName)
   --% usage.p_referenced_decl()
begin
   null;
end Test;
