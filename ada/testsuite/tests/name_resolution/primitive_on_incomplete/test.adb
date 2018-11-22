procedure Test is
   package Pouet is
      type Foo is tagged;

      procedure Do_Stuff (X : Foo) is null;

      type Foo is tagged null record;
   end Pouet;

   X : Pouet.Foo;
begin
   X.Do_Stuff;
end Test;
pragma Test_Block;
