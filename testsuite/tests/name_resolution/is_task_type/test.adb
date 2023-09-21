procedure Test is

   package Pkg is
      type Task_Attribute_Iface is task interface;
   end Pkg;

   use Pkg;

   procedure P (O : Task_Attribute_Iface'Class) is
   begin
      abort O;
      pragma Test_Statement;
   end P;

begin
   null;
end Test;
