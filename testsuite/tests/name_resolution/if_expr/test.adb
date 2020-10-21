procedure Test is
   A, B, C : Integer;

   function Foo return Integer;
   function Foo return Boolean;

begin
   A := (if True then B else C);
   pragma Test_Statement;

   A := (if Foo then Foo else C);
   pragma Test_Statement;
end Test;
