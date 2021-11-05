procedure Main is
   type Arr is array (Integer range 1 .. 3) of Integer;
   type Rec is record
      A : Arr;
   end record;
   type Rec_Arr is array (Integer range 1 .. 3) of Rec;

   procedure Foo (A : in Arr; B : out Arr) is
   begin
      null;
   end Foo;

   package Pkg is
      type T is tagged record
         Bar : access procedure (X : in out Integer);
      end record;

      procedure Foo (X : in out T) is null;
   end Pkg;

   X : Arr;
   Y : aliased Integer := 2;
   Z : access Integer := Y'Access;
   V : Rec;
   W : Rec_Arr;
   R : Pkg.T;
begin
   X := (others => Y);
   X (1) := Y;
   Foo (X, X);
   Z.all := 2;
   V.A := X;
   W (2).A (2) := W (1).A (1);
   Foo (W (Y).A, W (Y).A);
   R.Foo;
   R.Bar (Y);
   Pkg.Foo (R);
   V := (A => X);
end Main;
