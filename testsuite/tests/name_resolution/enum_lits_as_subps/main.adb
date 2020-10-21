procedure Main is
   type A is (B, C, D, E);

   function Foo return A renames B;
   pragma Test_Statement;

   generic
      type T is (<>);
      with function Baz return T;
   package Bar is end Bar;

   package Bar_Inst is new Bar (A, B);
   pragma Test_Statement;

begin
   null;
end Main;
