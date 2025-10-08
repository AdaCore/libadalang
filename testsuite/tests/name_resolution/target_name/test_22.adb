pragma Ada_2022;

procedure Test_22 is
   type Arr is array (1 .. 10) of Integer;

   A : Arr := (others => 1);
begin
   A := [@ with delta 2 => @(2) + 1];
   pragma Test_Statement;
end Test_22;
