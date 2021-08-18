package Test is
   package Pack is
      G_count : Integer;
   end Pack;

   type T is tagged null record;

   function F (Dummy : T; Unused : Integer) return Boolean;

   function Pr1 (X : in out T; Y : in out Integer) return Integer;
   pragma Pre (X.F (30) = False);
   pragma Post (X.F (30)'Old = X.F (20)
                and then Test.Pack.G_count'Old > 0
                and then Y'Old > Y
                and then Pr1'Result = Bar (Y));
   pragma Test_Case
     (Name => "Pr test 1",
      Mode => Nominal,
      Requires => X.F (30) = False,
      Ensures => X.F (30)'Old = X.F (20)
                 and then Test.Pack.G_count'Old > 0
                 and then Y'Old > Y
                 and then Pr1'Result = Bar (Y));

   function Bar (X : Integer) return Integer is (X);
end Test;
pragma Test_Block;
