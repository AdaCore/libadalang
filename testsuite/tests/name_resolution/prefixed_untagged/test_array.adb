procedure Test_Array is
   package Pkg is
      type T is null record;
      type T_Array is array (Positive range <>) of T;

      procedure Foo (Self : T_Array) is null;
   end Pkg;

   use Pkg;

   X : T_Array (1 .. 10);
begin
   X.Foo;
   pragma Test_Statement;
end Test_Array;

