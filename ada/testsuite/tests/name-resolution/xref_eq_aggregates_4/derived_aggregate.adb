with Ada.Text_IO; use Ada.Text_IO;

procedure Derived_Aggregate is
   type Float is delta 0.01 digits 10;
   type Integer is range 1 .. 10;

   type Rec is tagged record
      A, B : Float;
   end record;

   type Rec_2 is new Rec with record
      C, D : Integer;
      R : Rec;
   end record;

   R : Rec_2;
   function Foo return Integer is (2);
   function Foo return Float is (2.0);
begin
   R := (A => 1.0, B => 2.0, C => 3, D => 4, R => (1.0, Foo));
   pragma Test_Statement;
end Derived_Aggregate;
