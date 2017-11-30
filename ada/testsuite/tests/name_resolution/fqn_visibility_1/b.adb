package body B is
   Body_Int : Integer := 12;

   I : Integer := B.Public_Int + B.Private_Int + B.Body_Int;
   pragma Test_Statement;

   procedure Foo is null;
end B;
