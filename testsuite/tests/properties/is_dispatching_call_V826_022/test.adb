procedure Test is
   package A is
      type T is tagged null record;

      type T_Access is access all T'Class;
      procedure Foo (O : T_Access) is null;
   end A;

   procedure Bar (O : A.T_Access) is
   begin
      A.Foo (O);
      --% node.f_call.p_is_dispatching_call()
   end Bar;
begin
   null;
end Test;
