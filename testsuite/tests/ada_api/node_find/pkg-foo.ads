package Pkg.Foo is

   type Foo is private;

   function "+" (Left, Right : Foo) return Foo;

   package Nested is
      Bar, Foo : Integer;
   end Nested;

   package 'C' is
   end;

private

   type Foo is record
      Foo : Boolean;
   end record;

end Pkg.Foo;
