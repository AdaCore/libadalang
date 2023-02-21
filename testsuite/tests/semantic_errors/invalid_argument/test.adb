procedure Test is
   procedure Foo (X : Integer) is null;
begin
   Foo (True);
   pragma Test_Statement;
end Test;
