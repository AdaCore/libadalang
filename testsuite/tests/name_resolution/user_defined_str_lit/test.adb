procedure Test is
   package Pkg is
      type Str is null record
         with String_Literal => To_Str;
      pragma Test_Block;

      function To_Str (X : Wide_Wide_String) return Str is
        (null record);
      function To_Str (X : String) return Str is (null record);
   end Pkg;

   X : Pkg.Str := "Helloooo";
   pragma Test_Statement;
begin
   null;
end Test;
