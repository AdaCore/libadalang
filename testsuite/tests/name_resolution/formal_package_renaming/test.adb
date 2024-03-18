procedure Test is
   generic
      with procedure Foo is <>;
   package Op_G is
   end Op_G;


   generic
      with package Ops is new Op_G (<>);
   package Cont is
      procedure Bar;
   end Cont;

   package body Cont is
      package O renames Ops;

      procedure Bar is
      begin
         O.Foo;
      end Bar;
   end Cont;

   procedure Foo is null;
   package My_Ops is new Op_G;
   package My_Cont is new COnt (My_Ops);
   pragma Test_Statement;
begin
   null;
end Test;
