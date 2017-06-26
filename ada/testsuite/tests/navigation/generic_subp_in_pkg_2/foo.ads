package Foo is

   procedure Put_Line (S : String);

   generic
      type T is private;
      with procedure To_String (Inst : T) return String;
   procedure Print_T (Inst : T);

   procedure Print_Int is new Print_Int_Gen (Put_Line);

end Foo;
