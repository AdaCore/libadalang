with Foo.Gen;

package Bar is
   package Inst is new Foo.Gen (Integer);
   pragma Test (Inst);
end Bar;
