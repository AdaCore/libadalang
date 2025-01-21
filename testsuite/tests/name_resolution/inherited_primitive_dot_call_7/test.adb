procedure Test is
   generic
      type D is private;
   package Gen is
      type T is tagged null record;

      procedure Foo (Self : T; Data : D) is null;
   end Gen;

   generic
      type U is tagged private;
   package Gen_Wrapper is
      type T is new U with null record;
   end Gen_Wrapper;

   package Pkg is
      type T is tagged private;
      type D is null record;

      procedure Bar;
   private
      package My_Gen is new Gen (D);
      package My_Wrap is new Gen_Wrapper (My_Gen.T);

      type T is new My_Wrap.T with null record;
   end Pkg;

   package body Pkg is
      procedure Bar is
         Elem : My_Wrap.T;
         Data : D;
      begin
         Elem.Foo (Data);
         pragma Test_Statement;
         My_Wrap.Foo (Elem, Data);
         pragma Test_Statement;
      end Bar;
   end Pkg;
begin
   null;
end Test;
