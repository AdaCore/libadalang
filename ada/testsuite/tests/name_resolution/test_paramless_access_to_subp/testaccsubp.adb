procedure Testaccsubp is
   type Fn_Ptr is access function return Integer;

   function A return Integer is (12);

   A_Ptr : Fn_Ptr := A'Access;

   I : Integer;
begin

   I := A_Ptr.all;
   pragma Test_Statement;

   pragma Test (A_Ptr);
   pragma Test (A_Ptr.all);
end Testaccsubp;
