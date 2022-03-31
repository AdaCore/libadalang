procedure Test is
   function "+" (X : Integer) return Integer is
   begin
      return 0;
   end;

   B : Boolean;
begin
   --  The unary `+` below should not resolve to the user-defined `+` operator
   --  above, even though it's the most visible one. That's because the unary
   --  `+` operator on `root_integer` takes precedence in ambiguous cases.
   B := 1.0 * (+1) in 0.0 .. 0.0;
   pragma Test_Statement;
end Test;
