with Ada.Text_IO; use Ada.Text_IO;

pragma Extensions_Allowed (On);

procedure Test_Format is
begin
   Put_Line (f"Hello, 12 + 15 = {12 + 15}");
   pragma Test_Statement;

   Put_Line (f"Hello, 12 + 15 = {String'("lol")}");
   pragma Test_Statement;

   Put_Line (f"Hello, 12 + 15 = {"lol"}");
   pragma Test_Statement (Expect_Fail => True);

end Test_Format;
