package Test is

   type My_Int   is range 0 .. 20_000;
   type My_Float is digits 12;

   type My_Record is record
      A, B : My_Int;
      F    : My_Float;
   end record;

   procedure Foo is
      E : My_Float;
      G : My_Record;
   begin
      E := G.F;
      pragma Test_Statement;
   end Foo;

end Test;
