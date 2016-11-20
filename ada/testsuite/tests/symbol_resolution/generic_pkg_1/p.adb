package body P is
   procedure Foo (Self : T) is
      New_T : T;
   begin
      New_T := Self;
      pragma Test_Statement;
   end Foo;
end P;
