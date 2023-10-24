procedure Test is

   package Pkg is
      type Window is tagged private
         with Constant_Indexing => CRef,
              Variable_Indexing => VRef;

      type Color is (R, G, B);

      type Pixel is record
         C : Color;
         X, Y : Natural;
      end record;

      subtype Name is String(1..6);

      type Ref_Rec (D : access Pixel) is null record
         with Implicit_Dereference => D;

      function CRef (W : in Window; N : in Name) return Pixel;

      function VRef (W : aliased in out Window; N : in Name) return Ref_Rec;

      function CRef (W : in Window; X, Y : in Natural) return Pixel;

      function VRef (W : aliased in out Window;
                     X, Y : in Natural) return Ref_Rec;

   private

      type PArray is array (1..10) of aliased Pixel;
      type Window is tagged record
         Count : Natural := 0;
         Pixels : PArray;
      end record;
   end Pkg;

   package body Pkg is

      function CRef (W : in Window; X, Y : in Natural) return Pixel is
      begin
         return W.Pixels (1);
      end CRef;

      function VRef (W : aliased in out Window;
                     X, Y : in Natural) return Ref_Rec is
      begin
         return (D => W.Pixels (1)'Access);
      end VRef;

      function CRef (W : in Window; N : in Name) return Pixel is
      begin
         return W.Pixels (1);
      end CRef;

      function VRef (W : aliased in out Window; N : in Name) return Ref_Rec is
      begin
         return (D => W.Pixels (1)'Access);
      end VRef;

   end Pkg;

   use Pkg;

   procedure P (W : in out Window; X, Y : Natural) is
      procedure Ident (C : Color) is null;
   begin
      Ident (W (X, Y).C);
      pragma Test_Statement;

      Ident (W.CRef (X, Y).C);
      pragma Test_Statement;

      Ident (W ("ABCDEF").C);
      pragma Test_Statement;

      W (X, Y).C := R;
      pragma Test_Statement;

      W ("ABCDEF").C := G;
      pragma Test_Statement;
   end P;

begin
   null;
end Test;
