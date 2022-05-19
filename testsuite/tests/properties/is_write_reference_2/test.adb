procedure Test is
   procedure Foo (X : out Natural) is
   begin
      X := 0;
   end Foo;

   procedure Bar (X : out Integer) is
   begin
      Foo (Integer (X));
      --% foo_arg = node.f_call.f_suffix[0].f_r_expr
      --% foo_arg.p_is_write_reference()
      --% x_arg = foo_arg.f_suffix[0].f_r_expr
      --% x_arg.p_is_write_reference()
   end Bar;
begin
   null;
end Test;
