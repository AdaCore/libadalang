package B.C is
private
   Private_Int : Integer := B.Public_Int + B.Private_Int;
   pragma Test_Statement;
end B.C;
