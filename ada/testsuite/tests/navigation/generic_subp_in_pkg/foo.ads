package Foo is

   procedure Put_Line (S : String);

   generic
      with procedure Put_Line (S : String);
   procedure Print_Int_Gen (I : Integer);

   procedure Print_Int is new Print_Int_Gen (Put_Line);

end Foo;
