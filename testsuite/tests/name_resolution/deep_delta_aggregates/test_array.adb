procedure Test_Array with SPARK_Mode is
   type Point is record
      X, Y : Integer;
   end record;

   type Segment is array (1 .. 2) of Point;
   S : Segment;

   type Triangle is array (1 .. 3) of Segment;
   T : Triangle;

   type Cube is array (1 .. 3, 1 .. 4) of Segment;
   C : Cube;
begin
   S := (S with delta (1) => (1, 2));
   pragma Test_Statement;

   S := (S with delta (1).X => 1);
   pragma Test_Statement;

   S := (S with delta (1).X | (2).Y => S(2).X, (1).Y => S(2).Y);
   pragma Test_Statement;

   T := (T with delta (2)(1).Y => T(1)(2).X);
   pragma Test_Statement;

   C := (C with delta (1, 2)(3).Y => 1);
   pragma Test_Statement;
end Test_Array;
