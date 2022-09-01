with System.Storage_Elements;

procedure Test is
   X : System.Address := System'To_Address (2);
   pragma Test_Statement_UID;

   generic
      type Param_Type is private;
      type Return_Type is private;
      with function Foo (X : Param_Type) return Return_Type;
   package Pkg_G is
   end Pkg_G;

   package My_Pkg_To_Address is new Pkg_G
     (System.Storage_Elements.Integer_Address,
      System.Address,
      System'To_Address);
   pragma Test_Statement_UID;
begin
   null;
end Test;

