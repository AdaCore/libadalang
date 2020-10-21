procedure Test is
   Value : constant Integer := 10;
   Var   : Integer := 1;

   generic
      X : in out Integer;
      Y : in Integer;
   package P is
      procedure Foo;
   end P;

   package body P is
      procedure Foo is
      begin
         X := Y;
      end Foo;
   end P;

   package P_1 is new P (X => Var, Y => Value);
   package P_2 is new P (X => Var, Y => 13);
begin
   P_1.Foo;
   P_2.Foo;
end Test;
