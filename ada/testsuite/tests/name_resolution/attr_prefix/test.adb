procedure Test is
   package P is
      type T is new Integer;
   end P;

   X : P.T := P.T'First;
   pragma Test_Statement;
begin
   null;
end Test;
