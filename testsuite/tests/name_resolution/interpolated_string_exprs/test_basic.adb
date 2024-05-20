with Ada.Text_IO; use Ada.Text_IO;

pragma Extensions_Allowed (On);

procedure Test_Basic is
   type Rec is record
      A, B : Integer;
   end record;

   S : String := "Cruel";
begin
   Put_Line(f"Hello {S} world {S & S} world");
   pragma Test_Statement;
   Put_Line(f"1 + 2 = {1 + 2}");
   pragma Test_Statement;
   Put_Line(f"Rec = {Rec'(13, 15)}");
   pragma Test_Statement;
end Test_Basic;
