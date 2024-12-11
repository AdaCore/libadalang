procedure Test is
   type Acc is access procedure;

   procedure Foo is null;

   A : Acc := Fo'Access;
   pragma Test_Statement (Expect_Fail => True);
begin
   null;
end Test;
