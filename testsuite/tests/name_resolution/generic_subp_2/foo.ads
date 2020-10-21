package Foo is

   function To_String (I : Integer) return String;

   generic
      type T is private;
      with function To_String (Inst : T) return String;
   procedure Print_T (Inst : T);

   procedure Print_Int is new Print_T (Integer, To_String);

end Foo;
