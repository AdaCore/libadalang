with System;

procedure Test_Subp_Address is
   function Foo (A : Integer) return Integer is (A * 2);

   A : System.Address := Foo'Address;
   pragma Test_Statement;
begin
   null;
end Test_Subp_Address;
