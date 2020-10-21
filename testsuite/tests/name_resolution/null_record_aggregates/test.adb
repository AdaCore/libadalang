procedure Test is
   type Base is tagged null record;

   type Derived is new Base with null record;

   No_Base : constant Base := (null record);

   No_Derived_1 : constant Derived := (Base with null record);
   pragma Test_Statement;

   No_Derived_2 : constant Derived := (No_Base with null record);
   pragma Test_Statement;

begin
   null;
end Test;
