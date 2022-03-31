procedure Test is
   generic
      type T is private;
   package Refs is
      type Reference_Type (Element : access T)
         is limited null record
         with Implicit_Dereference => Element;
   end Refs;

   package Pkg is
      type T is tagged null record;

      package T_Refs is new Refs (T);

      function Get return T_Refs.Reference_Type is
        (Element => new T'(null record));
   end Pkg;

   B : Boolean;
begin
   B := Pkg.Get in Pkg.T;
   pragma Test_Statement;
end Test;

