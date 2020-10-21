procedure Test2 is
   generic
      A : Integer;
   package Foo is
      type Arr is array (1 .. A) of Positive;
   end Foo;

   package Bar is new Foo (12);

   A : Bar.Arr := (1 => 2, others => 15);
   pragma Test_Statement;
begin
   null;
end Test2;
