procedure Multidim with SPARK_Mode is
   type Segment is array (1 .. 2) of Integer;
   S : Segment;

   type Point is record
      X, Y : Segment;
   end record;

   type Triangle is array (1 .. 3) of Point;
   T : Triangle;

   type Multi is array (1 .. 3, Boolean) of Triangle;
   M : Multi;
begin
   M := (M with delta (1, True) (1).Y (2) => 1);
   pragma Test_Statement;
end Multidim;
