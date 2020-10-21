package body Foo.Bar is
   procedure Test (X : access U_Record) is
      Y : access T_Record'Class;
   begin
      Y := X;
      pragma Test_Statement;
   end Test;
end Foo.Bar;
