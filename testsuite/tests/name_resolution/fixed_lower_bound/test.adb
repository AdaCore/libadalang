--  Check that the following experimental GNAT feature:
--    Fixed lower bounds for array types and subtypes
--  is correctly supported.

--  https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-fixed-lower-bound.rst
pragma Extensions_Allowed (On);

procedure Test is
   type A is array (Integer range 1 .. <>) of Integer;
   pragma Test_Block;

   type String_1 is array (Positive range 1 .. <>) of Character;
   pragma Test_Block;

   subtype Fixed_String is String (1 .. <>);
   pragma Test_Statement;

   subtype Fixed_String_2 is String (Integer range 1 .. <>);
   pragma Test_Statement;

   type Int_Matrix_1 is array (Positive range 1 .. <>,
                               Positive range <>) of Integer;
   pragma Test_Block;
   type Int_Matrix_2 is array (Positive range <>,
                               Positive range 1 .. <>) of Integer;
   pragma Test_Block;
   type Int_Matrix_3 is array (Positive range 1 .. <>,
                               Positive range 1 .. <>) of Integer;
   pragma Test_Block;

   type Matrix is array (Positive range <>, Positive range <>) of Integer;
   pragma Test_Block;

   subtype SMatrix is Matrix (2 .. <>, 2 .. <>);
   pragma Test_Statement;
   subtype SMatrix2 is Matrix (Integer range 2 .. <>, Integer range 2 .. <>);
   pragma Test_Statement;
begin
   null;
end Test;
