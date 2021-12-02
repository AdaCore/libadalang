with Foo;
procedure Test is
   package My_Foo is new Foo (Integer);
   --% node.p_designated_generic_decl.p_body_part_for_decl()
begin
   null;
end Test;
