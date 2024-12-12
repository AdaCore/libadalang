package Accesses is
   type Acc1 is access constant Integer;

   type Rec1 is record
      I : aliased Integer;
      A : access Integer;
   end record;

   procedure Proc1 (A : access Integer) is null;

   type Arr1 is array (Boolean) of access Integer;
end Accesses;
