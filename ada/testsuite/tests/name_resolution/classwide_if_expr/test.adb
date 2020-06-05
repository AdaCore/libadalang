procedure Test is
   package P is
      type T is tagged null record;
      type U is new T with null record;
   end P;

   X : P.T'Class := P.T'(null record);
   Y : P.U'Class := P.U'(null record);
   Z : P.T'Class := (if True then X else Y);
   pragma Test_Statement;

   W : P.T'Class := (if True then X elsif True then Y else Y);
   pragma Test_Statement;
begin
end Test;
