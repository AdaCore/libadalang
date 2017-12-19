with Bar;

generic
   type Counter is private;
   Init : Counter;
package Foo is
   package Bar_Inst is new Bar (Counter);
   package Bar_Gen_Inst is new Bar_Inst.Gen (Init);
end Foo;
