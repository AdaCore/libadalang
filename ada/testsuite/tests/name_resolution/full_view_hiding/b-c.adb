package body B.C is
   I2 : Typ := (3, 4);

   Inst3 : Typ := I2 and (3, 4);
   pragma Test_Statement;

   Inst4 : Typ := Wat (I2, (3, 4));

   procedure Foo is null;
end B.C;
