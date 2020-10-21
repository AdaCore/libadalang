package body Foo is
   procedure Main (X : T) is
      R : Positive;
   begin
      R := X.Bar'Length;
      pragma Test_Statement;
   end Main;
end Foo;
