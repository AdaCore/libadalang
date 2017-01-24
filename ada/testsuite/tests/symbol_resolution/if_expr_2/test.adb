procedure Test is
   A, B, C : Integer;

   function Foo return Integer;
   function Foo return Boolean;

begin
   pragma Section("Test elsif branches");
   A := (if Foo then Foo elsif True then B else C);
   pragma Test_Statement;

   pragma Section("Test single then");
   A := (if (if Foo then Foo) then Foo else B);
   pragma Test_Statement;
end Test;
