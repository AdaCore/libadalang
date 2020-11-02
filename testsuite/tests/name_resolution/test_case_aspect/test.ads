package Test is
   package Pack is
      G_count : Integer;
   end Pack;

   type T is tagged null record;

   function F (Dummy : T; Unused : Integer) return Boolean;

   procedure Pr1 (X : in out T; Y : Integer) with
     Test_Case => (Name => "Pr test 1",
                   Mode => Nominal,
                   Requires => X.F (30) = False,
                   Ensures  => X.F (30)'Old = X.F (20)
                               and then Test.Pack.G_count'Old > 0
                               and then Y'Old > Y);

   function Pr2 (X : String) return String with
      Test_Case => ("Example", Nominal);
end Test;
pragma Test_Block;
