package body Foo is
   procedure Proc is
   begin
      pragma Test (Bar.Helper);
   end Proc;
end Foo;
