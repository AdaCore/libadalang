with Interfaces.C.Strings;

procedure Test is
   generic
      type T is private;
   procedure Foo (X : T);

   procedure Foo (X : T) is
   begin
      Foo (X);
      --% node.f_call.p_is_dispatching_call()
   end Foo;

   type F_Access is access function return Interfaces.C.Strings.chars_ptr;
   F : F_Access;
   Str : String := Interfaces.C.Strings.Value (F.all);
   --% node.f_default_expr.p_is_dispatching_call()
begin
   null;
end Test;
