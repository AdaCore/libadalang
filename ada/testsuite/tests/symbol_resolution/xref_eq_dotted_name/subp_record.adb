procedure Subp_Record is
   type Integer is range 0 .. 10000;

   type Rec_2 is record
      D : Integer;
   end record;

   type Rec_1 is record
      C : Integer;
   end record;

   type Rec_A is record
      B : Rec_1;
   end record;

   type Rec_B is record
      B : Rec_2;
   end record;

   function Foo return Rec_A is (B => (C => 12));
   function Foo return Rec_B is (B => (D => 12));

   Pouet : Integer;
begin
   Pouet := Foo.B.C;
   pragma Test_Statement;

   Pouet := Foo.B.D;
   pragma Test_Statement;
end Subp_Record;
