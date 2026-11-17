procedure Test is
   type Rec is record
      X : Integer;
   end Rec;

   procedure Foo (R : Rec) is null;

   R : Rec;
begin
   Foo (R.);
   pragma Test_Statement (Expect_Fail => True);
   --  This test is only to ensure that we don't crash with an exception: we
   --  don't expect a very helpful semantic diagnostic.
end Test;
