procedure Test is
   X : Integer;
   B : Boolean := ">=" (Left => X, Right => X);
   pragma Test_Statement;
begin
   null;
end Test;
