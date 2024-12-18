procedure Test is
   generic
      type T is private;
   package Vectors is
      type Vector is tagged null record;

      function Is_Empty (Self : Vector) return Boolean is (False);
   end Vectors;

   generic
      type T is private;
   package My_Vectors is
      package Std_Vectors is new Vectors (T);

      type Vector is new Std_Vectors.Vector with null record;
   end My_Vectors;

   package Int_Vectors is new My_Vectors (Integer);

   type Vector_Access is access all Int_Vectors.Vector;

   X : Vector_Access;
   B : Boolean;
begin
   B := X.Is_Empty;
   pragma Test_Statement;
end Test;
