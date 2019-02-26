package Pkg is
   type T is tagged null record;


   procedure Classic_Prim (X : access T'Class);

private

   procedure Private_Prim (X : access T'Class);

end Pkg;
