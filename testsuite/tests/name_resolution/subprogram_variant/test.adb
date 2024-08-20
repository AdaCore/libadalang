with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Test is
   function Incr (I : Integer) return Boolean
   is (I > 0)
   with Subprogram_Variant => (Increases => I + 1);
   pragma Test_Block;

   function Decr (I : Integer) return Boolean
   is (I > 0)
   with Subprogram_Variant => (Decreases => -(I + 1));
   pragma Test_Block;

   function Decr_Big_Int (I : Integer) return Boolean
   is (I > 0)
   with Subprogram_Variant => (Decreases  => To_Big_Integer (I));
   pragma Test_Block;

   function Multi (I, J : Integer) return Boolean
   is (I > 0)
   with Subprogram_Variant => (Increases => I, Decreases => J);
   pragma Test_Block;

   function Structural (I : Integer) return Boolean
   is (I > 0)
   with Subprogram_Variant => (Structural => I);
   pragma Test_Block;
begin
   null;
end Test;
