procedure Test is
   procedure Foo (A : Integer) is null;
   procedure Foo (A : Boolean) is null;
   task type T is
      entry E;
   end T;

   task body T is
   begin
      Foo (E'Count);
      pragma Test_Statement;
   end T;

   Inst : T;
begin
   Foo (Inst'Callable);
   pragma Test_Statement;
   Foo (Inst'Terminated);
   pragma Test_Statement;
end Test;
