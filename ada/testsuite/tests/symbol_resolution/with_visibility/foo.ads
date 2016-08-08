with Bar;

package Foo is
   L : Integer;
   pragma Test (Bar.R);
   pragma Test (Bar.R.A);
   pragma Test (Bar.R.A.B);

   pragma Test (Bar.Pouet);
   pragma Test (Baz.Pouet);

   pragma Test (Baz.Loul);
end Foo;
