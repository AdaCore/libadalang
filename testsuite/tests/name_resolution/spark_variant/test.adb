with Ada.Numerics.Big_Numbers.Big_Integers;
with SPARK.Big_Integers;

procedure Test is
   procedure Foo
     (I : Integer;
      J : Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer := 0;
      K : SPARK.Big_Integers.Big_Integer := 0)
         with Ghost, Subprogram_Variant =>
           (Increases => I,
            Decreases => J,
            Decreases => K);
   pragma Test_Block;

   procedure Foo
     (I : Integer;
      J : Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer := 0;
      K : SPARK.Big_Integers.Big_Integer := 0)
   is
   begin
      null;
   end Foo;

   I : Integer := 0;
   J : Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer := 0;
   K : SPARK.Big_Integers.Big_Integer := 0;
begin
   while I < 10 loop
      pragma Loop_Variant (Increases => I);
      pragma Loop_Variant (Decreases => J);
      pragma Loop_Variant (Decreases => K);
      I := I + 1;
   end loop;
   pragma Test_Block;
end Test;
