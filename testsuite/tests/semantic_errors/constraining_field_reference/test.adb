procedure Test is
   type Rec1 is record
      X : Integer;
   end record;
   type Rec2 is record
      Y : Integer;
   end record;

   function Foo (X : Integer) return Rec1 is (X => 1);
   function Foo (X : Boolean) return Rec2 is (Y => 2);

   V : Integer := Foo (3).Y;
   pragma Test_Statement;
begin
   null;
end Test;
