procedure Main is

   package PT is
      type T is tagged null record;

      procedure Foo_1 (Self : T);
      procedure Foo_2 (X : Integer; Self : T);

      function Bar_1 (Self : T) return Integer is (1);
      function Bar_2 (Self : T; X : Integer) return Integer is (2);

      function Foo_Bar return T is (null record);
   end PT;

   package body PT is
      procedure Foo_1 (Self : T) is
      begin
         null;
      end Foo_1;
      procedure Foo_2 (X : Integer; Self : T) is
      begin
         null;
      end Foo_2;
   end PT;

   package PU is
      type U is new PT.T with null record;

      overriding procedure Foo_1 (Self : U);
      overriding procedure Foo_2 (X : Integer; Self : U);

      overriding function Bar_1 (Self : U) return Integer is (11);
      overriding function Bar_2 (Self : U; X : Integer) return Integer is (22);

      overriding function Foo_Bar return U is (null record);
   end PU;

   package body PU is
      procedure Foo_1 (Self : U) is
      begin
         null;
      end Foo_1;
      procedure Foo_2 (X : Integer; Self : U) is
      begin
         null;
      end Foo_2;
   end PU;

   use PT;

   A : PT.T;
   B : PU.U;
   X : PT.T'Class := B;
   I : Integer;

begin
   Foo_1 (X);
   Foo_2 (3, X);
   I := Bar_1 (X);
   I := Bar_2 (X, 3);
   X := Foo_Bar;

   X.Foo_1;
   I := X.Bar_1;
   I := X.Bar_2 (3);

   Foo_1 (A);
   Foo_2 (3, A);
   I := Bar_1 (A);
   I := Bar_2 (A, 3);
   A := Foo_Bar;

   A.Foo_1;
   I := A.Bar_1;
   I := A.Bar_2 (3);
end Main;
