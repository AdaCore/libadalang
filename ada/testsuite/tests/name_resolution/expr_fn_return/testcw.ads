package TestCW is
   type A is interface;

   type Root is tagged null record;

   type B is new Root and A with null record;

   C : constant B := (null record);

   function None return A'Class is (C);

   function Foo return Root'Class is (C);
end TestCW;
pragma Test_Block;
