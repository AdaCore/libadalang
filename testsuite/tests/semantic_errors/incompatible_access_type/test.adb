procedure Test is
   type Integer_Access is access Integer;

   X : aliased Boolean := True;
   Y : Integer_Access := X'Access;
   pragma Test_Statement (Expect_Fail => True);
begin
   null;
end Test;
