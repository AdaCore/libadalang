package Foo is
   function F (I : Integer) return Integer is (I);
   pragma Test_Statement;

   type Record_Type is record
      I, J : Integer;
   end record;

   function F (I : Integer) return Record_Type is ((I => I, J => I * 2));
   pragma Test_Statement;
end Foo;
