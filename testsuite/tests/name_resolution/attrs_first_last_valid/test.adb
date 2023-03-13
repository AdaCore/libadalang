procedure Test is
   type Enum is (A, B, C);

   subtype Sub_Enum is Enum
     with Static_Predicate => Sub_Enum in B;

   X : constant Sub_Enum := Sub_Enum'First_Valid;
   pragma Test_Statement;
   Y : constant Sub_Enum := Sub_Enum'Last_Valid;
   pragma Test_Statement;
begin
   null;
end Test;
