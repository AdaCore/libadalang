procedure Test is
   procedure Foo (A : Integer) is null;
   procedure Foo (A : Boolean) is null;
   task type T;

   task body T is
   begin
      null;
   end T;

   Inst : T;
begin
   Foo (Inst'Callable);
   pragma Test_Statement;
end Test;
