procedure Main is
   type Arr is array (Integer range 1 .. 3) of Integer;

   procedure Foo (A : in Arr; B : out Arr) is
   begin
      null;
   end Foo;

   X : Arr;
   Y : aliased Integer := 2;
   Z : access Integer := Y'Access;
begin
   X := (others => Y);
   X (1) := Y;
   Foo (X, X);
   Z.all := 2;
end Main;
