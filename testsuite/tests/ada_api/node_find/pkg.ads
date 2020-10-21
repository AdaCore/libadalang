package Pkg is

   type Foo is null record;

   No_Foo : constant Foo := (null record);
   Some_Foo : Pkg.Foo;

   function "+" (Left, Right : Foo) return Foo;

   No_Such_Foo : Pkg.Bar.Foo;

end Pkg;
