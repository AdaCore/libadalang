package Pkg.Foo is

   type Foo is private;

   function "+" (Left, Right : Foo) return Foo;

   package Nested is
      Bar, Foo : Integer;
   end Nested;

private

   type Foo is record
      Foo : Boolean;
   end record;

end Pkg.Foo;
