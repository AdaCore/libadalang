procedure Test is
   type Int is record
      Val : Integer;
   end record;

   package Pkg is
      type Map is tagged record
         E : aliased Int;
      end record
         with Variable_Indexing => Reference;

      type Reference_Type (Element : not null access Int) is
         null record
      with Implicit_Dereference => Element;

      function Reference (X : in out Map; I : Integer) return Reference_Type is
        ((Element => X.E'Access));

      type Map_Access is access all Map;
   end Pkg;

   M : Pkg.Map_Access := new Pkg.Map;
   I : Integer := M.all (3).Val;
   pragma Test_Statement;
begin
   null;
end Test;
