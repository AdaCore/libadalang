procedure Testcw is
   package Tag is
      type A is tagged record
         A : Integer;
      end record;

      procedure Foo (Self : A) is null;
   end Tag;

   use Tag;

begin

   declare
      CW : A'Class := A'(A => 12);
   begin
      CW.Foo;
   end;
   pragma Test_Block;

end Testcw;
