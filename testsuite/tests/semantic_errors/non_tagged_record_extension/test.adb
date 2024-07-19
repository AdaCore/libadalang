procedure Test is
   type T is record
      V : Integer;
   end record;

   X, Y : T;
begin
   X := (Y with V => 1);
   pragma Test_Statement (Expect_Fail => True);
end Test;
