with Ada.Text_IO; use Ada.Text_IO;

procedure Interpolation is
   X    : Integer := 12;
   Y    : Integer := 15;
   Name : String := "Leo";

   S : String (1 .. 4);
begin
   S := f"S1" & "S2";
   pragma Test_Statement;

   S := "S1" & "S2";
   pragma Test_Statement;

   S := f"S1" & f"S2";
   pragma Test_Statement;

   Put_Line (f"The name is {Name} and the" & f" sum is {X + Y}.");
   pragma Test_Statement;
end Interpolation;
