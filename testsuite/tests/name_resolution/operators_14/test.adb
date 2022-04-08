procedure A is
   B : Boolean;
   L : Integer;

   --  The expected type for the if expression and the literal should be
   --  `Integer`, even though the operation is done on Positives.
   X : Positive := 1 + (if B then L else 0);
   pragma Test_Statement;
begin
   null;
end;
