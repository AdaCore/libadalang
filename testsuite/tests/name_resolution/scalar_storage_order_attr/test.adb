with System; use System;

procedure Test is
   generic
      type T is private;
   package Pkg is
      function Foo return Boolean is
        (T'Scalar_Storage_Order = Low_Order_First);
      pragma Test_Statement_UID;

      type R is record
         C : T;
      end record;

      function Bar return Boolean is
        (R'Bit_Order = Low_Order_First);
      pragma Test_Statement_UID;

      function Baz return Boolean is
        (T'Scalar_Storage_Order = Standard'Default_Scalar_Storage_Order);
      pragma Test_Statement_UID;
   end Pkg;
begin
   null;
end Test;
