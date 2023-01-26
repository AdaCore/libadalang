package body Pkg is
   procedure Test is
      Y : Foo.T := Foo.Get;
      pragma Test_Statement;
   begin
      null;
   end Test;

   package body Nested is
      procedure Test is
      begin
         X := Foo.Get;
         pragma Test_Statement;
      end Test;
   end Nested;
end Pkg;
