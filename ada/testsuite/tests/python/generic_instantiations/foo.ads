package Foo is

   generic
      type T is private;
   package A is
      V : T;
   end;

   generic
      type T is private;
   package B is
      package A_Inst is new A (T);
   end B;

   generic
      type T is private;
   package C is
      package B_Inst is new B (T);
   end C;

   package C_Inst is new C (Integer);

   I : Integer := C_Inst.B_Inst.A_Inst.V;

end Foo;
