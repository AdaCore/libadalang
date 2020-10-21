procedure Test is
   X : Integer;

   procedure F (X : out Integer) is
   begin
      X := 12;
   end F;

   P : access procedure (X : out Integer) := F'Access;


   procedure Foo (X : access procedure (X : Integer)) is
   begin
      pragma Test (X);
   end Foo;
begin
   P (X);
   pragma Test_Statement;
end Test;
