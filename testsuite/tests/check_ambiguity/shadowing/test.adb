procedure Test is
   procedure Test_1 is
      procedure Foo (X : Integer) is null;
   begin
      declare
         procedure Foo (Y : Integer) is null;
      begin
         Foo (2);
         --  not ambiguous: Foo in declare block shadows the parent one
      end;
   end Test_1;

   package Test_2 is
      procedure Foo (X : Integer) is null;

      package Inner is
         procedure Foo (Y : Integer) is null;

         procedure Test;
      end Inner;
   end Test_2;

   package body Test_2 is
      package body Inner is
         procedure Test is
         begin
            Foo (2);
            --  not ambiguous: Test_2.Inner.Foo shadows Test_2.Foo
         end Test;
      end Inner;
   end Test_2;

   procedure Test_3 is
      use Test_2;
      use Inner;
   begin
      Foo (2);
      --  ambiguous: neither Test_2.Foo nor Test_2.Inner.Foo are declared in a
      --  parent scope of Test_3.
   end Test_3;

   package Test_4 is
      procedure Foo (X : Integer);

      procedure Test;
   end Test_4;

   package body Test_4 is
      procedure Foo (X : Integer) is
      begin
         raise Program_Error;
      end Foo;

      procedure Test is
         use Test_2;
      begin
         Foo (2);
         --  not ambiguous
      end Test;
   end Test_4;

   package Test_5 is
      package Inner is
         procedure Foo (X : Integer) is null;
         procedure Test;
      end Inner;
   end Test_5;

   package body Test_5 is
      procedure Foo (X : Integer) is
      begin
         raise Program_Error;
      end Foo;
      package body Inner  is
         procedure Test is
         begin
            Foo (2);
            --  not ambiguous: Test_5.Inner.Foo shadows Test_5.Foo
         end Test;
      end Inner;
   end Test_5;

   package Test_6 is
      package Prim is
         type T is null record;
         procedure Foo (X : T) is null;
      end Prim;

      package Inner is
         type U is new Prim.T;

         procedure Test;
      end Inner;

      procedure Foo (X : Inner.U);
   end Test_6;

   package body Test_6 is
      package body Inner is
         procedure Test is
            X : U;
         begin
            Foo (X);
            --  not ambiguous: the inherited primitive is declared
            -- "more recently".
         end Test;
      end Inner;

      procedure Foo (X : Inner.U) is
      begin
         raise Program_Error;
      end Foo;
   end Test_6;

   procedure Test_7 is
      X : Integer := 1;
   begin
      declare
         X : Integer := 2;
      begin
         X := 3;
         --  not ambiguous: X in declare block is more recent
      end;
   end Test_7;

   procedure Test_8 is
      Foo : constant Integer := 1;
   begin
      declare
         function Foo return Integer is (2);

         X : Integer;
      begin
         X := Foo;
         --  not ambiguous: Foo in declare block is more recent
      end;
   end Test_8;

   procedure Test_9 is
      function Foo return Integer is (1);
      function Foo return Boolean is (True);

      X : Integer;

      procedure Inner is
         X : Boolean;
      begin
         X := Foo;
         -- not ambiguous: even though no interpretation of Foo is more visible
         -- than another, X from Inner is and forces a particular Foo, so this
         -- is not ambiguous.
      end Inner;
   begin
      Inner;
   end Test_9;

   procedure Test_10 is
      function Foo return Integer is (0);

      package Tmp_1 is
         function Bar return Boolean is (False);
      end Tmp_1;

      package Tmp_2 is
         function Bar return Boolean is (True);
      end Tmp_2;

      procedure Test is
         use Tmp_1;
         use Tmp_2;

         function Foo return Integer is (1);

         procedure Baz (X : Integer; Y : Boolean) is null;

      begin
         Baz (Foo, Bar);
         -- ambiguous: while Foo from Test shadows Foo of the outer scope,
         -- there are two possible interpretations of Bar.
      end Test;
   begin
      null;
   end Test_10;
begin
   Test_10;
end Test;
