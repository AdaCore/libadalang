procedure Test is
   Value : constant Integer := 10;
   Var   : Integer := 1;

   generic
      X : in out Integer;
      Y : in Integer;
   procedure Foo;

   procedure Foo is
   begin
      X := Y;
   end Foo;

   procedure P_1 is new Foo (X => Var, Y => Value);
   procedure P_2 is new Foo (X => Var, Y => 13);
begin
   P_1;
   P_2;
end Test;
