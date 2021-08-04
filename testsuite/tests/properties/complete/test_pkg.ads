with Base;

package Test_Pkg is
   type A is new Base.Base_Type with null record;

   function My_Method (Self : A) return Integer;
end Test_Pkg;
