procedure Testaccesstosubp is
   type Pouet is access function (A : Integer) return Integer;

   function I (A : Integer) return Integer is (A * 2);

   A : Pouet := I'Access;

   N : Integer;
begin
   N := A (12);
   pragma Test_Statement;
end Testaccesstosubp;
