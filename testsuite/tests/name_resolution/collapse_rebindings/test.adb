procedure Test is
   generic
      type P is private;
   package Base_G is
      type T is tagged null record;

      procedure Foo (X : T; Y : P) is null;
   end Base_G;

   generic
      with package Base is new Base_G (<>);
   package Concrete_G is
      generic
         type U is new Base.T with private;
      package Inner_G is
         type V is new U with null record;

         procedure Test (X : V; Y : Base.P);
      end Inner_G;
   end Concrete_G;

   package body Concrete_G is
      package body Inner_G is
         procedure Test (X : V; Y : Base.P) is
         begin
            Foo (U (X), Y);
         end Test;
      end Inner_G;
   end Concrete_G;

   package My_Base is new Base_G (Integer);
   package My_Concrete is new Concrete_G (My_Base);
   package My_Inner is new My_Concrete.Inner_G (My_Base.T);
   pragma Test_Statement;
begin
   null;
end Test;
