procedure Test is
   package Pouet is
      type Foo is tagged;

      procedure Do_Stuff (X : Foo) is null;

      type Foo is tagged null record;
   end Pouet;

   package Bidule is
      type Foo is new Pouet.Foo with null record;

      procedure Bar;
   end Bidule;

   package body Bidule is
      procedure Bar is
         X : Foo;
      begin
         Do_Stuff (X);
         pragma Test_Statement;
      end Lol;
   end Bidule;

   X : Pouet.Foo;
begin
   X.Do_Stuff;
   pragma Test_Statement;
end Test;
