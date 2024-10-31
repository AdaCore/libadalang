with Ada.Task_Identification;

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

      accept E do
         declare
            I : Ada.Task_Identification.Task_Id :=
               E'Caller;
            pragma Test_Statement_UID;
         begin
            null;
         end;
      end E;
   end T;

   Inst : T;
begin
   Foo (Inst'Callable);
   pragma Test_Statement;
   Foo (Inst'Terminated);
   pragma Test_Statement;
end Test;
