with System;
with System.Storage_Pools;

procedure Storage_Pool_Attr is
   type A is access all Integer;

   B : A := new Integer'(21);
   Add : System.Address;

   S : System.Storage_Pools.Root_Storage_Pool'Class := B'Storage_Pool;
   pragma Test_Statement_UID;
begin
   S.Allocate (Add, 12, 16);
   pragma Test_Statement_UID;
end Storage_Pool_Attr;
