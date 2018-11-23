procedure Test is
   package Pouet is
      type Foo is tagged;

      type Foo_List is null record
         with Iterator_Element => Foo'Class;

      type Foo is tagged null record;

      procedure Bar (Self : Foo'Class);
   end Pouet;

   X : Pouet.Foo_List;
begin
   for V of X loop
      V.Bar;
   end loop;
end Test;
pragma Test_Block;
