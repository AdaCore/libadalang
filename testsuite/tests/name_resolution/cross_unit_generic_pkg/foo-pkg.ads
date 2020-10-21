generic
   type T is private;
package Foo.Bar is
   V : T;
   pragma Test (I);
   pragma Test (Foo.I);
end Foo.Bar;
