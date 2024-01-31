with Formal;
with Formal.Child;

procedure Test is
   generic
      with package Act is new Formal;
   package Gen is
      procedure Foo;
   end Gen;

   package body Gen is
      package Child_Act is new Act.Child;

      procedure Foo is
      begin
         Child_Act.Bar;
      end Foo;
   end Gen;

   package Package_Actual is new Formal;

   package Inst is new Gen (Package_Actual);
   pragma Test_Statement;
begin
   null;
end Test;
