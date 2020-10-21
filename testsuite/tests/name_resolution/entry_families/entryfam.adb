procedure Entryfam is
   type Typ is array (Integer range 1 .. 10) of Integer;

   protected type Pouet is
      entry A (Typ'Range) (B : Integer);
      entry U (B : Integer);
   end Pouet;

   protected body Pouet is
      entry A (for Spec in Typ'Range) (B : Integer)
      when True is
         C : Integer := Spec;
         pragma Test_Statement;
      begin
         null;
      end A;

      entry U (B : Integer) when True is begin null; end U;
   end Pouet;

   F : Pouet;
begin
   F.A (1) (2);
   pragma Test_Statement;
end Entryfam;
