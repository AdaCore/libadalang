procedure Test is
   type T is (A, B, C);

   X : T;
begin
   case X is
      when T range A .. B =>
         null;
      when others =>
         null;
   end case;
   pragma Test_Statement;
end Test;
