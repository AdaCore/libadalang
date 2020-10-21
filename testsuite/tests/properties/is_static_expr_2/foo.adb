procedure Foo is
   type B is range 1 .. 20;
   C : B;
begin
   for A in B'Range loop
      null;
   end loop;
   --% $node.f_spec.f_iter_expr.p_is_static_expr()
   --
   for A in B'First .. B'Last loop
      null;
   end loop;
   --% $node.f_spec.f_iter_expr.p_is_static_expr()

   for A in B'First .. C loop
      null;
   end loop;
   --% $node.f_spec.f_iter_expr.p_is_static_expr()
end Foo;
