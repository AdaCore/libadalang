with Ada.Text_IO; use Ada.Text_IO;

procedure Test is

   generic
      with procedure Foo;
   procedure Bar;

   procedure Bar is
   begin
      Foo;
   end Bar;

   type Proc_Access is access procedure;

   My_Foo_Access : Proc_Access := null;

   procedure Bar_Inst is new Bar (My_Foo_Access.all);
   pragma Test_Statement;
begin
   null;
end Test;
