procedure Simple_Aggregate is
   type Integer is range 1 .. 10;
   type Rec is record
      A, B : Integer;
   end record;

   R : Rec;
   function Foo return Integer;
   function Foo return Float;
begin
   R := (1, 2);
   pragma Test_Statement;

   R := (1, 2.0);
   pragma Test_Statement;

   R := (1, Foo);
   pragma Test_Statement;
end Simple_Aggregate;
