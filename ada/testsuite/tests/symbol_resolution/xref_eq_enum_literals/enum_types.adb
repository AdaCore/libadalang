procedure Test is
   type A is (Enum_1, Enum_2);
   type B is (Enum_1, Enum_2, Enum_3);

   P : A;
   Q : B;
begin
   P := Enum_1;
   pragma Test_Statement;

   Q := Enum_1;
   pragma Test_Statement;

   P := Enum_3;
   pragma Test_Statement;
end Test;
