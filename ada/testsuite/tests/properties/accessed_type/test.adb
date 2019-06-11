procedure Test is
   type A is access all Integer;

   type B (C : access Natural) is null record
      with Implicit_Dereference => C;

   C : access Positive;
begin
   null;
end Test;
