procedure Test is
   generic
   package Array_G is
      type T is null record;

      generic
      procedure Iterate_G (X : T);
   end Array_G;

   package body Array_G is
      procedure Iterate_G (X : T) is null;
   end Array_G;

   package Internal_Address_Set is new Array_G;

   type U is new Internal_Address_Set.T;

   generic
   procedure Iterate_G (F : U);

   procedure Iterate_G (F : U) is
      procedure Iterate is new Internal_Address_Set.Iterate_G;
   begin
      Iterate (Internal_Address_Set.T (F));
   end Iterate_G;

   procedure Iterate is new Iterate_G;
   pragma Test_Statement;
begin
   null;
end Test;

