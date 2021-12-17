package body Protect is
   protected body T is
      procedure Proc is
      begin
         X := X + 1;
      end Proc;

      function Func return Integer is
      begin
         return X;
         --% node.f_return_expr.p_is_constant
      end Func;

      function Foo (A : in out Integer) return Integer is
      begin
         return A;
         --% node.f_return_expr.p_is_constant
      end Foo;
   end T;
end Protect;
