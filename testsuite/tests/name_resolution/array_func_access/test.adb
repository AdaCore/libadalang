procedure Main is
   type Arr is array (Integer range 1 .. 2) of Integer;
   type Fun_Access is access function (X : Arr) return Arr;

   My_Fun : Fun_Access;
   My_Arr : Arr;
begin
   My_Arr := My_Fun (My_Arr);
   pragma Test_Statement;
end Main;
