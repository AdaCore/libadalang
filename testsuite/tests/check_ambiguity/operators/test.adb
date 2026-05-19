procedure Test is
   procedure Test_1 is
      X : Integer;
   begin
      X := X + 2;
   end Test_1;

   procedure Test_2 is
      X : constant := 1 + 2;
   begin
      null;
   end Test_2;

   procedure Test_3 is
      package Test is
         function "+" (X, Y : Integer) return Integer is (X);

         X : constant := 1 + 2;
         Y : constant Integer := 1 + 2;
      end Test;

      use Test;

      Z : constant Integer := 1 + 2;
   begin
      null;
   end Test_3;

   procedure Test_4 is
      type T is null record;

      function "+" (X, Y : T) return Integer is (0);
      function "-" (X : T) return Integer is (0);

      procedure Foo (X : T) is null;
      procedure Foo (X : Integer) is null;

      X : T;
   begin
      Foo (X + X);
      Foo (-X);
   end Test_4;
begin
   null;
end Test;
