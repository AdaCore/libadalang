procedure Testfirst is
   type A is array (Integer range <>) of Integer;
   type B is new Integer range 1 .. 5872395;

   C   : B;
   D   : A := (1, 2, 3, 4, 5);
   Idx : Integer;

   type Lol is record
      Pouet : A (1 .. 10);
   end record;

   function Foo return Lol is (others => <>);
   function Foo return Integer is (12);
begin
   C := B'First;
   Idx := D'First;

   Idx := Foo.Pouet'First;
   D (D'First) := 2;
end Testfirst;
pragma Test_Block;
