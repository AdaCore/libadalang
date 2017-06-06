procedure Testcw is
   package Tag is
      type A is tagged record
         A : Integer;
      end record;

      procedure Foo (Self : A) is null;
      function Foo (Self : A) return Integer;
   end Tag;

   use Tag;

begin

   declare
      CW : A'Class := A'(A => 12);
   begin
      Foo (Cw);
      pragma Test_Statement;
   end;

end Testcw;
