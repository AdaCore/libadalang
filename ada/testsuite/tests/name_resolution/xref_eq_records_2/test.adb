package Test is

   type My_Int   is range 0 .. 20_000;
   type My_Float is digits 12;

   type My_Record is record
      A, B : My_Int;
      F    : My_Float;
   end record;

   procedure Foo is
      C, D, E : My_Float;
      Z : My_Int;
      G : My_Record;

      function Bar (A : My_Int; B : My_Float) return My_Float;
      function Bar (A : My_Float; B : My_Record) return My_Float;

      function Baz return My_Int;
      function Baz return My_Float;

   begin
      begin
         C := Bar (Baz, G);
         D := Bar (Baz, Baz);
         E := G.F;
      end;
      pragma Test_Block;
   end Foo;

end Test;
