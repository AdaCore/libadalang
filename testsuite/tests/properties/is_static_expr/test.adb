procedure Test_It (Param : Integer; Param_2 : String) is
   A : constant Integer := 10;
   B : constant Integer := 5;
   C : constant Boolean := True;

   Test_1 : constant Integer := A + B;
   Test_2 : constant Integer := Test_1;
   Test_3 : constant Integer := (if Test_1 = 0 then 1 else 2);
   Test_4 : constant Integer := Param;

   Test_5 : constant Boolean := not C;

   D : constant String := "hello";

   Test_6 : constant Boolean := D'Length > 0;

   E : constant Boolean := D'Length < 3;

   F : constant Boolean := D'Length < Param_2'Length;
begin
   null;
end Test_It;
