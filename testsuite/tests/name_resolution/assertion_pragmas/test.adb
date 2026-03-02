procedure Test is
   X : Integer;
begin
   --  Assert

   pragma Assert (X > 0, "0");
   pragma Test_Statement;

   pragma Assert (X > 0, Message => "0");
   pragma Test_Statement;

   pragma Assert (Check => X > 0, Message => "0");
   pragma Test_Statement;

   pragma Assert (Runtime => X > 0, Static => X > 0);
   pragma Test_Statement;

   --  Assert_And_Cut

   pragma Assert_And_Cut (X > 1, "1");
   pragma Test_Statement;

   pragma Assert_And_Cut (Runtime => X > 1);
   pragma Test_Statement;

   --  Assume

   pragma Assume (X > 2, "2");
   pragma Test_Statement;

   pragma Assume (Runtime => X > 2);
   pragma Test_Statement;
end Test;
