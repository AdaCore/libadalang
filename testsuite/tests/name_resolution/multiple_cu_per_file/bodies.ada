with Pkg2;

package body Pkg1 is
   function Fact (I : Integer) return Integer is
   begin
      if I < 2 then
         return 1;
      else
         return I * Pkg2.Id (Fact (I - 1));
         pragma Test_Statement;
      end if;
   end Fact;
end Pkg1;

package body Pkg2 is
   function Id (I : Integer) return Integer is
   begin
      return I;
   end Id;
end Pkg2;
