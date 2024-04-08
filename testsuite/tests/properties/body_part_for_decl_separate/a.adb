procedure A is
   package P is
      procedure Bar;
      --% node.p_body_part_for_decl()
   private
      procedure Foo;
      --% node.p_body_part_for_decl()
   end P;

   package body P is separate;
begin
   null;
end A;
