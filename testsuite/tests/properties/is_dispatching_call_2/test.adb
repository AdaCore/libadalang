procedure Test is
   generic
      type T is private;
   procedure Foo (X : T);

   procedure Foo (X : T) is
   begin
      Foo (X);
      --% node.f_call.p_is_dispatching_call()
   end Foo;
begin
   null;
end Test;
