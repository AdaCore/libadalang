procedure Prim_Ret is
   package A is
      type Base is null record;
      function Create return Base is (null record);
   end A;

   type Derived is new A.Base;

   Inst : Derived;
begin

   Inst := Create;
end Prim_Ret;
pragma Test_Block;
