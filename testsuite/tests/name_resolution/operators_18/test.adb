--  Resolution of the return statement below used to fail because the solver
--  contradiction generated for a topo sort failure while attempting an
--  incorrect branch used to prevent the correct branch from being chosen
--  later on.

procedure Test is
   type Uint8 is mod 2 ** 8;

   function Foo (Left, Right : Uint8) return Boolean is
   begin
      return ((Left + 1) + (Right + 2) + 3) = 4;
      pragma Test_Statement;
   end Foo;
begin
   null;
end Test;
