procedure Test is
   type Pouet is range 1 .. 100;
   P : Pouet;
   Q : Integer := 14;
begin
   P := Pouet (Q);
   pragma Test_Statement;
end Test;
