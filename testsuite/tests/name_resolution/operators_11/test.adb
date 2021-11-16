procedure Test is
   generic
      type T is range <>;
   procedure Foo;

   procedure Foo is
      type U is new T'Base range 0 .. 10;

      UX, UY : U := 0;
      UZ : U := "+" (UX, UY);
      pragma Test_Statement;
   begin
      null;
   end Foo;
begin
   null;
end Test;
