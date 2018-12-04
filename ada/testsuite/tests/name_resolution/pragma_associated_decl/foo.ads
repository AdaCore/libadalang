package Foo is
   type T is private;

private
   type T is null record;
   pragma Pack (T);
   pragma Test_Statement;
end Foo;
