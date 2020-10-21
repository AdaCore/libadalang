package Foo is
   type T is record
      B : Boolean;
   end record;

   for T use record
      B at 0 range 0 .. 0;
   end record;
end Foo;
