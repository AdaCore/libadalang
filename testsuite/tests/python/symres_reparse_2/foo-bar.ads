package Foo.Bar is
   B : Integer := 1;

   pragma Test (A);
   pragma Test (Foo.Bar.B);
   pragma Test (NN);
end Foo.Bar;
