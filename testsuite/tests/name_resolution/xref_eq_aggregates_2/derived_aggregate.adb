with Ada.Text_IO; use Ada.Text_IO;

procedure Derived_Aggregate is
   type Float is delta 0.01 digits 10;
   type Integer is range 1 .. 10;

   type Rec is tagged record
      A, B : Float;
   end record;

   type Rec_2 is new Rec with record
      C, D : Integer;
   end record;

   R : Rec_2;
   function Foo return Integer is (2);
   function Foo return Float is (2.0);
begin
   R := (1.0, 2.0, 3, 4);
   pragma Test_Statement;

   R := (C => 3, D => 4, B => 1.0, A =>2.0);
   pragma Test_Statement;

   R := (C | D => 3, B => 1.0, A =>2.0);
   pragma Test_Statement;

   R := (3, 4, 1.0, 2.0);
   pragma Test_Statement (Expect_Fail => True);
end Derived_Aggregate;
