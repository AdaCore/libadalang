package Pkg is

   type T is tagged null record;

   procedure Bar (X : access T'Class);

end Pkg;
