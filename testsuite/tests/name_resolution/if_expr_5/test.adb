procedure Test is
   type Base is new Positive;
   subtype Base_Or_Null is Base'Base range 0 .. Base'Last;

   B : constant Base := 1;
   X : Base_Or_Null := (if True then B else Base_Or_Null'Last);
   pragma Test_Statement;
begin
   null;
end Test;
