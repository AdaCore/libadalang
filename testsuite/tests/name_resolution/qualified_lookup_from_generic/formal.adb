package body Formal is
   X : Integer;

   procedure Baz is
   begin
      Formal.X := 42;
   end Baz;
end Formal;

