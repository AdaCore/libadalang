procedure Test_It (Param : Integer) is
   A : constant Integer := 10;
   B : constant Integer := 5;

   Test_1 : constant Integer := A + B;
   Test_2 : constant Integer := Test_1;
   Test_3 : constant Integer := (if Test_1 = 0 then 1 else 2);
   Test_4 : constant Integer := Param;
begin
   null;
end Test_It;
