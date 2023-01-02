procedure Test is
   package Foo is
      type T is null record;
   end Foo;

   Foo : Foo.T;
   --% node.f_type_expr.p_designated_type_decl
   -- This is invalid Ada but should not make LAL crash!
begin
   null;
end Test;
